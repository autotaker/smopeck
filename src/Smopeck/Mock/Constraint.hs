{-# LANGUAGE DeriveFunctor #-}
module Smopeck.Mock.Constraint where

import           Smopeck.Mock.Location
import           Smopeck.Spec.Syntax

data RangeF a = RangeF {
    rangeFrom :: a
    , rangeTo :: a
} deriving(Eq,Ord,Show,Functor)

type RangeExp = RangeF Exp
data Exp = Exp deriving (Show)

data Constraint =
    CType Location TypeExp  -- s : typExp
    | CRange Location RangeExp Constraint -- forall v <- range. c

getLocation :: Constraint -> Location
getLocation (CType loc _)    = loc
getLocation (CRange loc _ _) = loc
