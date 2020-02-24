{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module Smopeck.Logic.Model where

import           Control.Monad.Primitive
import           Smopeck.Mock.Value
import           Smopeck.Spec.Exp
import           System.Random.MWC

class Model m where
    data Atom m
    join :: Atom m -> Atom m -> Atom m
    meet :: Atom m -> Atom m -> Atom m
    top  :: Atom m
    bot  :: Atom m
    generate :: PrimMonad f => Gen (PrimState f) -> Atom m -> f (Maybe m)
    interpret :: Op -> Literal Desugar -> Atom m
