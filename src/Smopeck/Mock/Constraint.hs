{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Smopeck.Mock.Constraint where

import qualified Data.Map                as M
import           Data.Scientific
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
type Assertion = Exp
type Predicate = (Op, Exp)

type DNF a = Lattice Join (Lattice Meet a)

data Constraint =
    CType {
        cLocation   :: !Location,
        cShape      :: !TypeExp,
        cPredicates :: !(DNF Predicate),
        cAssertion  :: !Assertion
      }
    | CRange {
        cLocation     :: !Location,
        cRange        :: !RangeExp,
        cBodyLocation :: !(T.LocationExp Desugar),
        cBodyType     :: !TypeExp
      }

type SolveM a = IO a

generateNumber :: DNF (Op, Scientific) -> SolveM Scientific
generateNumber _ = pure 0

chooseShape :: TypeExp -> SolveM TypeExpF
chooseShape _ = pure undefined

solveConstraint :: Assignment -> Constraint -> SolveM (Value, [Constraint], [DepEdge])
solveConstraint assign CType{..} = do
  let ctx = M.mapMaybe (\case
        VObject _ -> Nothing
        VArray -> Nothing
        VNumber v -> pure $ LNumber v
        VString v -> pure $ LString v
        VBool v -> pure $ LBool v
        VNull -> pure LNull) assign
  ty <- chooseShape cShape
  case T.typeExpName ty of
    T.Prim T.PNumber -> do
      let evalNumber e = case eval ctx (evalLocation e) of
            Left l | Just (VNumber v) <- M.lookup l assign -> v
            Right (LNumber v) -> v
            v -> error $ "expected number but found:" ++ show v
          evalLocation :: ExpF Desugar (T.LocationExp Desugar) -> ExpF Desugar Location
          evalLocation = fmap evalLocationExp
          evalLocationExp :: T.LocationExp Desugar -> Location
          evalLocationExp = fmap evalIndex
          evalIndex :: T.Exp Desugar -> Int
          evalIndex e =
            case eval ctx (evalLocation e) of
              Left l | Just (VNumber v) <- M.lookup l assign -> floor l
              Right (LNumber l) -> floor l
              l -> error $ "expected number but found:" ++ show l

      let ePredicates = fmap (fmap (\(op, e) -> (op, evalNumber e))) cPredicates
      v <- generateNumber ePredicates
      pure (VNumber v, [], [])





