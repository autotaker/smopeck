{-# LANGUAGE TypeFamilies #-}
module Smopeck.Logic.Number where

import           Control.Monad.Primitive
import           Data.Scientific
import           System.Random.MWC

class Model m where
    data Atom m
    join :: Atom m -> Atom m -> Atom m
    meet :: Atom m -> Atom m -> Atom m
    top  :: Atom m
    bot  :: Atom m
    generate :: PrimMonad f => Gen (PrimState f) -> Atom m -> f (Maybe m)

data End a = Open | Inclusive a | Exclusive a
data Range a = Range { leftEnd :: End a,  rightEnd :: End a }
type RangeArea a = [Range a]

instance Model Scientific where
    newtype Atom Scientific = RangeArea [Range Scientific]
    top = RangeArea [Range Open Open]
    bot = RangeArea []
    join = undefined
    meet = undefined
    generate = undefined
