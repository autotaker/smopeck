{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveFunctor #-}
module Smopeck.Mock.Constraint where

import           Smopeck.Mock.Location
import           Smopeck.Spec.Exp
import           Smopeck.Spec.Syntax   hiding (Exp)

data RangeF a = RangeF {
    rangeFrom :: !a,
    rangeTo   :: !a
} deriving(Eq,Ord,Show,Functor)

type RangeExp = RangeF Exp
newtype Exp = Exp (ExpF Desugar (LocationF Root Exp))
  deriving(Eq, Ord, Show)
type Assertion = Exp
type Predicate = (Op, Exp)

type DNF a = [[a]]

data Constraint =
    CType {
        cLocation   :: !Location,
        cShape      :: !Shape,
        cPredicates :: !(DNF Predicate),
        cAssertion  :: !Assertion
      }
    | CRange {
        cLocation :: !Location,
        cRange    :: !RangeExp,
        cType     :: !TypeExp
      } deriving(Eq, Ord, Show)

data Shape =
    SNull | SNumber | SString
    | SObject {
        shapeBindName :: !Location,
        shapeFields   :: [(FieldName, TypeExp)]
      }
    | SArray {
        shapeBindName   :: !Location,
        shapeLengthType :: !TypeExp,
        shapeIndexName  :: !Location,
        shapeIndexType  :: !TypeExp
      }
      deriving(Show, Eq, Ord)

