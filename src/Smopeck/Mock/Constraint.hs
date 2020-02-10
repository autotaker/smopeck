{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module Smopeck.Mock.Constraint where

import           Control.Monad.Reader
import           Data.Bifunctor
import           Data.Function
import qualified Data.HashTable.IO       as H
import qualified Data.Map                as M
import           Data.Scientific
import qualified Data.Set                as S
import           Smopeck.Mock.Dependency
import           Smopeck.Mock.Location
import           Smopeck.Mock.Value
import           Smopeck.Spec.Exp
import           Smopeck.Spec.Lattice
import qualified Smopeck.Spec.TypeExp    as T
data RangeF a = RangeF {
    rangeFrom :: !a,
    rangeTo   :: !a
} deriving(Eq,Ord,Show,Functor)

type RangeExp = RangeF Exp
type Exp = T.Exp Desugar
type TypeExp = T.TypeExp Desugar T.WHNF
type TypeExpF = T.TypeExpF Desugar T.WHNF
type TypeEnv = T.WHNFTypeEnv Desugar
type Assertion = Exp
type Predicate = (Op, Exp)
type LocationExp = T.LocationExp Desugar


data Constraint =
    CType {
        cLocation  :: !Location,
        cShape     :: !TypeExp,
        cAssertion :: !Assertion
      }
    | CRange {
        cLocation     :: !Location,
        cRange        :: !RangeExp,
        cBodyLocation :: !LocationExp,
        cBodyType     :: !TypeExp
      }


type HashTable k v = H.BasicHashTable k v
type SolveM a = ReaderT Context IO a

data Context = Context {
  assignment :: HashTable Location (Either TypeExp Value),
  typeEnv    :: TypeEnv
}

generateNumber :: Lattice Full (Op, Scientific) -> SolveM Scientific
generateNumber _ = pure 0

chooseShape :: TypeExp -> SolveM TypeExpF
chooseShape _ = pure undefined

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
evalNumber e = undefined


parent :: Location -> Location
parent (p `Get` _)   = p
parent (p `Field` _) = p
parent (Root _)      = error "no parent for root"

solveConstraint :: TypeEnv -> Assignment -> Constraint -> SolveM (Value, [Constraint], [DepEdge])
solveConstraint env assign CType{..} = do
  let ctx = M.mapMaybe (\case
        VObject _ -> Nothing
        VArray -> Nothing
        VNumber v -> pure $ LNumber v
        VString v -> pure $ LString v
        VBool v -> pure $ LBool v
        VNull -> pure LNull) assign
  let evalNumber :: Exp -> Scientific
      evalNumber e = case eval ctx (evalLocation e) of
        Left l | Just (VNumber v) <- M.lookup l assign -> v
        Right (LNumber v) -> v
        v -> error $ "expected number but found:" ++ show v
      evalLocation :: Exp -> ExpF Desugar Location
      evalLocation (T.Exp e) = fmap (fmap (floor . evalNumber)) e
  ty <- chooseShape cShape
  case T.typeExpName ty of
    T.Prim T.PNumber -> do
      let predicates =
              T.typeExpRef ty
                & filter (\case { (Root (), _, _) -> True; _ -> False})
                & map (\(_, op, e) -> LElem (op, evalNumber e))
                & foldr LMeet LTop
      v <- generateNumber predicates
      pure (VNumber v, [], [])
    T.Prim T.PObject -> do
      let exts = T.typeExpExt ty
          fields = M.keysSet exts
          refs = T.typeExpRef ty
          cs = M.toList exts & map (\(field, tyExp) ->
            CType {
              cLocation = cLocation `Field` field,
              cShape = T.evalTypeExp env tyExp,
              cAssertion = T.Exp (Literal (LBool True))
            })
          depVars = do
            CType{cShape = tyExp} <- cs
            let shape =
                  tyExp
                  & fmap (\ x -> S.unions $ do
                      (_, _, T.Exp e) <- T.typeExpRef x
                      pure (locations e))
                  & cata CataJoin{
                      fJBot = S.empty,
                      fJElem = id,
                      fJJoin = S.union
                    }
                  & S.toList
                  & concatMap decompDep
                  & S.fromList
                  & S.toList
            pure ()


      pure (VObject fields, cs, [])


decompDep :: LocationExp -> [LocationBlob]
decompDep = undefined







