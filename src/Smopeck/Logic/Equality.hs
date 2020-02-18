{-# LANGUAGE TypeFamilies #-}
module Smopeck.Logic.Equality where

import qualified Data.Set            as S
import           Smopeck.Logic.Model
import           System.Random.MWC

newtype Equality a = Equality a

instance (Variate a, Ord a) => Model (Equality a) where
    data Atom (Equality a) = Any | OneOf !(S.Set a)
    top = Any
    bot = OneOf S.empty
    join Any _                 = Any
    join _ Any                 = Any
    join (OneOf xs) (OneOf ys) = OneOf (S.union xs ys)
    meet Any y                 = y
    meet x Any                 = x
    meet (OneOf xs) (OneOf ys) = OneOf (S.intersection xs ys)
    generate st Any = do
        x <- uniform st
        pure $ Just $! Equality x
    generate st (OneOf xs)
        | S.null xs = pure Nothing
        | otherwise = do
            i <- uniformR (0,S.size xs - 1) st
            pure $ Just $! Equality $ S.elemAt i xs
