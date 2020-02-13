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


-- return true if rightEnd intersect with leftEnd
intersectEnd :: Ord a => End a -> End a -> Bool
intersectEnd Open _                      = True
intersectEnd _ Open                      = True
intersectEnd (Inclusive a) (Inclusive b) = a >= b
intersectEnd (Inclusive a) (Exclusive b) = a > b
intersectEnd (Exclusive a) (Inclusive b) = a > b
intersectEnd (Exclusive a) (Exclusive b) = a > b

intersect :: Ord a => Range a -> Range a -> Bool
intersect (Range l1 r1) (Range l2 r2) = intersectEnd r1 l2 && intersectEnd r2 l1

joinRange :: Ord a => Range a -> Range a -> Range a
joinRange (Range l1 r1) (Range l2 r2) = Range (minEnd l1 l2) (maxEnd r1 r2)
    where
    minEnd Open _ = Open
    minEnd _ Open = Open
    minEnd (Inclusive a) (Inclusive b) = Inclusive (min a b)
    minEnd (Exclusive a) (Exclusive b) = Exclusive (min a b)
    minEnd (Inclusive a) (Exclusive b)
        | a <= b = Inclusive a
        | otherwise = Exclusive b
    minEnd (Exclusive b) (Inclusive a)
        | a <= b = Inclusive a
        | otherwise = Exclusive a
    maxEnd Open _ = Open
    maxEnd _ Open = Open
    maxEnd (Inclusive a) (Inclusive b) = Inclusive (max a b)
    maxEnd (Exclusive a) (Exclusive b) = Exclusive (max a b)
    maxEnd (Inclusive a) (Exclusive b)
        | a >= b = Inclusive a
        | otherwise = Exclusive b
    maxEnd (Exclusive b) (Inclusive a)
        | a >= b = Inclusive a
        | otherwise = Exclusive a


joinArea :: Ord a => RangeArea a -> RangeArea a -> RangeArea a
joinArea [] ys = ys
joinArea xs [] = xs
joinArea (x:xs) (y:ys)
    | intersect x y = joinArea (joinRange x y:xs) ys
    | leftEnd x `isLeftOf` leftEnd y = x : joinArea xs (y:ys)
    | otherwise = y : joinArea (x:xs) ys

isLeftOf :: Ord a => End a -> End a -> Bool
isLeftOf Open _                      = True
isLeftOf _ Open                      = False
isLeftOf (Inclusive a) (Inclusive b) = a <= b
isLeftOf (Inclusive a) (Exclusive b) = a <= b
isLeftOf (Exclusive a) (Inclusive b) = a < b
isLeftOf (Exclusive a) (Exclusive b) = a <= b
