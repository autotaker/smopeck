{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
module Smopeck.Mock.Constraint where

import           Control.Monad.Reader    hiding (join)
import qualified Data.Aeson              as JSON
import           Data.Bifunctor
import           Data.Foldable
import           Data.Function
import qualified Data.HashTable.IO       as H
import qualified Data.Map                as M
import           Data.Scientific
import qualified Data.Set                as S
import qualified Data.Text               as T
import           Smopeck.Logic.Model
import           Smopeck.Logic.Number
import           Smopeck.Mock.Dependency
import           Smopeck.Mock.Location
import           Smopeck.Mock.Value
import           Smopeck.Spec.Exp
import           Smopeck.Spec.Lattice
import qualified Smopeck.Spec.TypeExp    as T
import           System.Random.MWC

type Exp = T.Exp Desugar
type TypeExp = T.TypeExp Desugar T.WHNF
type TypeExpF = T.TypeExpF Desugar T.WHNF
type TypeEnv = T.WHNFTypeEnv Desugar
type Assertion = Exp
type Predicate = (Op, Exp)
type LocationExp = T.LocationExp Desugar


type HashTable k v = H.BasicHashTable k v
type SolveM a = ReaderT Context IO a

data Context = Context {
  assignment :: HashTable Location (Either TypeExp Value),
  typeEnv    :: TypeEnv,
  randState  :: GenIO
}

initContext :: TypeEnv -> IO Context
initContext env = Context <$> H.new <*> pure env <*> createSystemRandom

generateNumber :: Lattice Full (Op, Scientific) -> SolveM Scientific
generateNumber lat = do
  let g = cata CataFull{
            fFBot = bot,
            fFTop = top,
            fFJoin = join,
            fFMeet = meet,
            fFElem = toRange
          } lat
      toRange (op, v) = RangeAreaD (interpretRange op d)
        where
          d = case toBoundedRealFloat v of
            Left b  -> b
            Right b -> b
  gen <- asks randState
  r <- generate gen g
  case r of
    Just v  -> pure $ fromFloatDigits v
    Nothing -> error "no such value"

generateInt :: Lattice Full (Op, Scientific) -> SolveM Scientific
generateInt lat = do
  let g = cata CataFull{
            fFBot = bot,
            fFTop = top,
            fFJoin = join,
            fFMeet = meet,
            fFElem = toRange
          } lat
      toRange (op, v) = RangeAreaI (interpretRange op d)
        where
          d = floor v
  gen <- asks randState
  r <- generate gen g
  case r of
    Just v  -> pure $ fromIntegral v
    Nothing -> error "no such value"


chooseShape :: TypeExp -> SolveM TypeExpF
chooseShape ty = do
  gen <- asks randState
  let cands = toList ty
  idx <- uniformR (0, length cands - 1) gen
  pure $ cands !! idx


extract :: Location -> SolveM JSON.Value
extract loc = evalL loc >>= \case
  VBool b -> pure $ JSON.Bool  b
  VNumber n -> pure $ JSON.Number n
  VNull -> pure $ JSON.Null
  VString s -> pure $ JSON.toJSON s
  VObject fields -> do
    pairs <- mapM (\f -> do
      v <- extract (loc `Field` f)
      pure $ T.pack f JSON..= v) $ S.toList fields
    pure $ JSON.object pairs
  VArray -> do
    JSON.Number len <- extract $ loc `Field` "length"
    es <- forM [0..floor len - 1] $ extract . (Get loc)
    pure $ JSON.toJSON es

mockJson :: TypeEnv -> TypeExp -> IO JSON.Value
mockJson env ty = do
  ctx <- initContext env
  runReaderT doit ctx
    where
      root = Root (Absolute "it")
      doit = do
        tbl <- asks assignment
        liftIO $ H.insert tbl root (Left ty)
        extract root


evalL :: Location -> SolveM Value
evalL loc = do
  tbl <- asks assignment
  liftIO (H.lookup tbl loc) >>= \case
    Just (Right v) -> pure v
    Just (Left ty) -> do
      v <- evalT loc ty
      liftIO (H.insert tbl loc (Right v))
      pure v
    Nothing -> do
      evalL (parent loc)
      evalL loc

evalT :: Location -> TypeExp -> SolveM Value
evalT loc ty = do
  tyF <- chooseShape ty
  case T.typeExpName tyF of
    T.Prim T.PNumber -> do
      predicates <-
        T.typeExpRef tyF
          & filter (\case { (Root (), _, _) -> True; _ -> False})
          & mapM (\(_, op, e) -> LElem . (op, ) <$> evalNumber e)
      v <- generateNumber (foldr LMeet LTop predicates)
      pure (VNumber v)
    T.Prim T.PInt -> do
      predicates <-
        T.typeExpRef tyF
          & filter (\case { (Root (), _, _) -> True; _ -> False})
          & mapM (\(_, op, e) -> LElem . (op, ) <$> evalNumber e)
      v <- generateInt (foldr LMeet LTop predicates)
      pure (VNumber v)
    T.Prim T.PObject -> do
      let exts = T.typeExpExt tyF
          fields = M.keysSet exts
      forM_ (M.toList exts) $ \(field, tyExp) -> do
        env <- asks typeEnv
        let tyExp' = T.evalTypeExp env tyExp
            loc' = loc `Field` field
        tbl <- asks assignment
        liftIO (H.insert tbl loc' (Left tyExp'))
      pure (VObject fields)
    T.Prim T.PArray -> do
      env <- asks typeEnv
      let exts = T.typeExpExt tyF
          lenTy = T.evalTypeExp env (exts M.! "length")
          lenLoc = loc `Field` "length"
      VNumber len <- evalT (loc `Field` "length") lenTy
      tbl <- asks assignment
      liftIO (H.insert tbl lenLoc (Right (VNumber len)))
      forM_ [0..floor len - 1] $ \i -> do
        let getTy = T.evalTypeExp env (exts M.! "get")
            getLoc = loc `Get` i
        liftIO (H.insert tbl getLoc (Left getTy))
      pure VArray


evalNumber :: Exp -> SolveM Scientific
evalNumber e = toNumber <$> (evalExp e >>= deref)

deref :: Either Location (Literal Desugar) -> SolveM (Literal Desugar)
deref (Right l) = pure l
deref (Left x)  = toLiteral <$> evalL x

evalExp :: Exp -> SolveM (Either Location (Literal Desugar))
evalExp (T.Exp e) = go e
  where
  go (Literal l)   = pure (Right l)
  go (Var x)       = Left <$> traverse (fmap floor . evalNumber) x
  go (App op args) = Right . interpret op <$> mapM f args
    where
      f e = go e >>= \case
        Right l -> pure l
        Left x -> toLiteral <$> evalL x

toLiteral :: Value -> Literal Desugar
toLiteral (VBool b)   = LBool b
toLiteral (VNumber n) = LNumber n
toLiteral VNull       = LNull
toLiteral (VString s) = LString s
toLiteral (VObject _) = error "cannot convert object to literal"
toLiteral VArray      = error "cannot convert array to literal"

toNumber :: Literal m -> Scientific
toNumber (LNumber n) = n
toNumber _           = error "cannot convert to number"

parent :: Location -> Location
parent (p `Get` _)   = p
parent (p `Field` _) = p
parent (Root _)      = error "no parent for root"
