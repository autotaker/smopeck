{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Smopeck.Mock.Constraint where

import           Data.Bifunctor
import           Data.Function
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


type SolveM a = IO a

generateNumber :: Lattice Full (Op, Scientific) -> SolveM Scientific
generateNumber _ = pure 0

chooseShape :: TypeExp -> SolveM TypeExpF
chooseShape _ = pure undefined

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







