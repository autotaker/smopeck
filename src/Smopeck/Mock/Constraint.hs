{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module Smopeck.Mock.Constraint where

import           Control.Monad.Reader   hiding (join)
import qualified Data.Aeson             as JSON
import qualified Data.Aeson.Key         as K
import qualified Data.Aeson.KeyMap      as KM
import           Data.Bifunctor
import           Data.Foldable
import           Data.Function
import qualified Data.HashMap.Strict    as HM
import qualified Data.HashTable.IO      as H
import qualified Data.Map               as M
import           Data.Scientific
import qualified Data.Set               as S
import           Data.String            (fromString)
import qualified Data.Text              as T
import qualified Data.Vector            as V
import           Smopeck.Logic.Equality
import           Smopeck.Logic.Model
import           Smopeck.Logic.Number
import           Smopeck.Logic.Regex
import           Smopeck.Mock.Location
import           Smopeck.Mock.Value
import qualified Smopeck.Spec.Exp       as Exp
import           Smopeck.Spec.Exp       hiding (interpret)
import           Smopeck.Spec.Lattice
import qualified Smopeck.Spec.TypeExp   as T
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

data Context = Context
  { assignment :: HashTable ALocation (Either TypeExp Value),
    typeEnv    :: TypeEnv,
    randState  :: GenIO
  }

initContext :: TypeEnv -> IO Context
initContext env = Context <$> H.new <*> pure env <*> createSystemRandom

genInstance :: Model m => Lattice Full (Op, Literal Desugar) -> SolveM m
genInstance lat = do
  let g =
        cata
          CataFull
            { fFBot = bot,
              fFTop = top,
              fFJoin = join,
              fFMeet = meet,
              fFElem = uncurry interpret
            }
          lat
  gen <- asks randState
  r <- generate gen g
  case r of
    Just v  -> pure v
    Nothing -> error "no such value"

chooseShape :: ALocation -> TypeExp -> SolveM TypeExpF
chooseShape base ty = do
  gen <- asks randState
  cands <-
    filterM
      ( \tyf ->
          case T.typeExpCond tyf of
            T.HasCond e -> do
              v <- evalExp base e >>= deref base
              case v of
                LBool True -> pure True
                LBool False -> pure False
                _ -> error $ "condition " ++ show e ++ " is evaluated to non Boolean: " ++ show v
            T.NoCond -> pure True
      )
      $ toList ty
  when (null cands) $ error $ "no candidates: " ++ show (base, ty)
  idx <- uniformR (0, length cands - 1) gen
  pure $ cands !! idx

extract :: ALocation -> SolveM JSON.Value
extract loc =
  evalL loc (castRoot loc) >>= \case
    VBool b -> pure $ JSON.Bool b
    VNumber n -> pure $ JSON.Number n
    VNull -> pure JSON.Null
    VString s -> pure $ JSON.toJSON s
    VObject fields -> do
      pairs <-
        mapM
          ( \f -> do
              v <- extract (loc `Field` f)
              pure $ fromString f JSON..= v
          )
          $ S.toList fields
      pure $ JSON.object pairs
    VArray -> do
      JSON.Number len <- extract $ loc `Field` "length"
      es <- forM [0 .. floor len - 1] $ extract . Get loc
      pure $ JSON.toJSON es

mockJson :: TypeEnv -> TypeExp -> IO JSON.Value
mockJson env ty = initContext env >>= mockJsonWithContext env ty

mockJsonWithContext :: TypeEnv -> TypeExp -> Context -> IO JSON.Value
mockJsonWithContext env ty = runReaderT doit
  where
    root = Root (Absolute "")
    doit = do
      tbl <- asks assignment
      liftIO $ H.insert tbl root (Left ty)
      extract root

mockJsonWithEnv :: TypeEnv -> M.Map String JSON.Value -> TypeExp -> IO JSON.Value
mockJsonWithEnv env venv ty = do
  ctx <- initContext env
  let insertValue k v = H.insert (assignment ctx) k (Right v)
      insert key (JSON.Object obj) = do
        let keys = map K.toString $ KM.keys obj
        insertValue key $ VObject $ S.fromList keys
        forM_ (KM.toList obj) $
          uncurry $
            insert . Field key . K.toString
      insert key (JSON.Array arr) = do
        let size = V.length arr
        insertValue (key `Field` "length") $
          VNumber $
            fromIntegral size
        V.imapM_ (insert . Get key) arr
      insert key (JSON.Number n) = insertValue key $ VNumber n
      insert key (JSON.Bool b) = insertValue key $ VBool b
      insert key (JSON.String s) =
        insertValue key $ VString (T.unpack s)
      insert key JSON.Null = insertValue key VNull

  forM_ (M.assocs venv) $
    uncurry $
      insert . Root . Absolute
  mockJsonWithContext env ty ctx

evalL :: ALocation -> Location -> SolveM Value
evalL base loc' =
  case resolve base loc' of
    Right i -> pure $ VNumber (fromIntegral i)
    Left loc -> do
      tbl <- asks assignment
      liftIO (H.lookup tbl loc) >>= \case
        Just (Right v) -> pure v
        Just (Left ty) -> do
          v <- evalT loc ty
          liftIO (H.insert tbl loc (Right v))
          pure v
        Nothing -> do
          evalL base (parent loc')
          evalL base loc'

evalT :: ALocation -> TypeExp -> SolveM Value
evalT loc ty = do
  tyF <- chooseShape loc ty
  case T.typeExpName tyF of
    T.Prim T.PNumber -> do
      predicates <-
        T.typeExpRef tyF
          & mapM (\(op, e) -> LElem . (op,) <$> (evalExp loc e >>= deref loc))
      v <- genInstance (foldr LMeet LTop predicates)
      pure (VNumber (fromFloatDigits (v :: Double)))
    T.Prim T.PInt -> do
      predicates <-
        T.typeExpRef tyF
          & mapM (\(op, e) -> LElem . (op,) <$> (evalExp loc e >>= deref loc))
      v <- genInstance (foldr LMeet LTop predicates)
      pure (VNumber (fromIntegral (v :: Int)))
    T.Prim T.PString -> do
      predicates <-
        T.typeExpRef tyF
          & mapM (\(op, e) -> LElem . (op,) <$> (evalExp loc e >>= deref loc))
      v <- genInstance (foldr LMeet LTop predicates)
      pure (VString v)
    T.Prim T.PBool -> do
      predicates <-
        T.typeExpRef tyF
          & mapM (\(op, e) -> LElem . (op,) <$> (evalExp loc e >>= deref loc))
      (Equality v) <- genInstance (foldr LMeet LTop predicates)
      pure (VBool v)
    T.Prim T.PNull -> pure VNull
    T.Prim T.PObject -> do
      let exts = T.typeExpExt tyF
          fields = S.fromList [field | FieldString field <- S.toList $ M.keysSet exts]
      forM_ (M.toList exts) $ \(FieldString field, tyExp) -> do
        env <- asks typeEnv
        let tyExp' = T.evalTypeExp env tyExp
            loc' = loc `Field` field
        tbl <- asks assignment
        liftIO (H.insert tbl loc' (Left tyExp'))
      pure (VObject fields)
    T.Prim T.PArray -> do
      env <- asks typeEnv
      let exts = T.typeExpExt tyF
          lenTy = T.evalTypeExp env (exts M.! FieldString "length")
          lenLoc = loc `Field` "length"
      VNumber len <- evalT (loc `Field` "length") lenTy
      tbl <- asks assignment
      liftIO (H.insert tbl lenLoc (Right (VNumber len)))
      forM_ [0 .. floor len - 1] $ \i -> do
        let getTy = T.evalTypeExp env (exts M.! FieldIndex T.BindDebrujin)
            getLoc = loc `Get` i
        liftIO (H.insert tbl getLoc (Left getTy))
      pure VArray

evalNumber :: ALocation -> Exp -> SolveM Scientific
evalNumber base e = toNumber <$> (evalExp base e >>= deref base)

deref :: ALocation -> Either Location (Literal Desugar) -> SolveM (Literal Desugar)
deref _ (Right l)   = pure l
deref base (Left x) = toLiteral <$> evalL base x

evalExp :: ALocation -> Exp -> SolveM (Either Location (Literal Desugar))
evalExp base (T.Exp e) = go e
  where
    go (Literal l) = pure (Right l)
    go (Var x) = Left <$> traverse (fmap floor . evalNumber base) x
    go (App op args) = Right . Exp.interpret op <$> mapM f args
      where
        f e =
          go e >>= \case
            Right l -> pure l
            Left x  -> toLiteral <$> evalL base x

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

parent :: LocationF (Root blob) Int -> LocationF (Root blob) Int
parent (p `Get` _)   = p
parent (p `Field` _) = p
parent (Root _)      = error "no parent for root"
