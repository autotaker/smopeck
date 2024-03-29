{-# LANGUAGE TypeFamilies #-}
module Smopeck.Logic.Number where

import           Control.Monad.Primitive
import           Data.Coerce
import           Data.Scientific
import           Smopeck.Logic.Model
import           Smopeck.Mock.Value
import           Smopeck.Spec.Exp        hiding (interpret)
import           System.Random.MWC

data End a = Open | Inclusive a | Exclusive a
    deriving(Eq,Ord,Show)
data Range a = Range { leftEnd :: LeftEnd a,  rightEnd :: RightEnd a }
    deriving(Eq,Ord,Show)
type RangeArea a = [Range a]

newtype LeftEnd a = LeftEnd (End a) deriving (Eq, Show)
newtype RightEnd a = RightEnd (End a) deriving (Eq, Show)

instance Ord a => Ord (LeftEnd a) where
    compare (LeftEnd a) (LeftEnd b) =
        case (a, b) of
            (Open, Open)               -> EQ
            (Open, _)                  -> LT
            (_, Open)                  -> GT
            (Inclusive a, Inclusive b) -> compare a b
            (Inclusive a, Exclusive b) -> if a <= b then LT else GT
            (Exclusive a, Inclusive b) -> if a < b then LT else GT
            (Exclusive a, Exclusive b) -> compare a b

instance Ord a => Ord (RightEnd a) where
    compare (RightEnd a) (RightEnd b) =
        case (a, b) of
            (Open, Open)               -> EQ
            (Open, _)                  -> GT
            (_, Open)                  -> LT
            (Inclusive a, Inclusive b) -> compare a b
            (Inclusive a, Exclusive b) -> if a >= b then GT else LT
            (Exclusive a, Inclusive b) -> if a > b then GT else LT
            (Exclusive a, Exclusive b) -> compare a b

instance Model Double where
    newtype Atom Double = RangeAreaD [Range Double]
    top = RangeAreaD [Range (LeftEnd Open) (RightEnd Open)]
    bot = RangeAreaD []
    join x y = RangeAreaD (joinArea (coerce x) (coerce y))
    meet x y = RangeAreaD (meetArea (coerce x) (coerce y))
    generate st xs = generateArea st eps (coerce xs)
        where eps x = abs x * 1e-53
    interpret Eq (LNumber v) =
        RangeAreaD [Range (LeftEnd (Inclusive d)) (RightEnd (Inclusive d))]
        where d = toDouble v
    interpret Lt (LNumber v) =
        RangeAreaD [Range (LeftEnd Open) (RightEnd (Exclusive d))]
        where d = toDouble v
    interpret Lte (LNumber v) =
        RangeAreaD [Range (LeftEnd Open) (RightEnd (Inclusive d))]
        where d = toDouble v
    interpret Gt (LNumber v) =
        RangeAreaD [Range (LeftEnd (Exclusive d)) (RightEnd Open)]
        where d = toDouble v
    interpret Gte (LNumber v) =
        RangeAreaD [Range (LeftEnd (Inclusive d)) (RightEnd Open)]
        where d = toDouble v
    interpret op v = error $ "cannot interpret: " ++ show (op, v)

toDouble :: Scientific -> Double
toDouble v = case toBoundedRealFloat v of
    Left v  -> v
    Right v -> v

instance Model Int where
    newtype Atom Int = RangeAreaI [Range Int]
    top = RangeAreaI [Range (LeftEnd Open) (RightEnd Open)]
    bot = RangeAreaI []
    join x y = RangeAreaI (joinArea (coerce x) (coerce y))
    meet x y = RangeAreaI (meetArea (coerce x) (coerce y))
    generate st xs = generateArea st eps (coerce xs)
        where eps x = 1
    interpret Eq v
        = meet (interpret Lte v) (interpret Gte v)
    interpret Lt (LNumber v) =
        RangeAreaI [Range (LeftEnd Open) (RightEnd (Inclusive i))]
        where i = ceiling (v - 1)
    interpret Lte (LNumber v) =
        RangeAreaI [Range (LeftEnd Open) (RightEnd (Inclusive i))]
        where i = floor v
    interpret Gt (LNumber v) =
        RangeAreaI [Range (LeftEnd (Inclusive i)) (RightEnd Open)]
        where i = floor (v + 1)
    interpret Gte (LNumber v) =
        RangeAreaI [Range (LeftEnd (Inclusive i)) (RightEnd Open)]
        where i = ceiling v

nullRange :: Ord a => Range a -> Bool
nullRange (Range (LeftEnd a) (RightEnd b)) =
    case (a, b) of
        (Open, _)                  -> False
        (_, Open)                  -> False
        (Inclusive a, Inclusive b) -> a > b
        (Inclusive a, Exclusive b) -> a >= b
        (Exclusive a, Inclusive b) -> a >= b
        (Exclusive a, Exclusive b) -> a >= b

interpretRange :: Op -> a -> [Range a]
interpretRange Eq v  = [Range (LeftEnd (Inclusive v)) (RightEnd (Inclusive v))]
interpretRange Lt v  = [Range (LeftEnd Open) (RightEnd (Exclusive v))]
interpretRange Lte v = [Range (LeftEnd Open) (RightEnd (Inclusive v))]
interpretRange Gt v  = [Range (LeftEnd (Exclusive v)) (RightEnd Open)]
interpretRange Gte v = [Range (LeftEnd (Inclusive v)) (RightEnd Open)]

intersect :: Ord a => Range a -> Range a -> Bool
intersect (Range l1 r1) (Range l2 r2) =
    not (nullRange (Range l2 r1)) &&
    not (nullRange (Range l1 r2))

joinRange :: Ord a => Range a -> Range a -> Range a
joinRange (Range l1 r1) (Range l2 r2) = Range (min l1 l2) (max r1 r2)

meetRange :: Ord a => Range a -> Range a -> Range a
meetRange (Range l1 r1) (Range l2 r2) = Range (max l1 l2) (min r1 r2)

joinArea :: Ord a => RangeArea a -> RangeArea a -> RangeArea a
joinArea [] ys = ys
joinArea xs [] = xs
joinArea (x:xs) (y:ys)
    | intersect x y = joinArea (joinRange x y:xs) ys
    | leftEnd x <= leftEnd y = x : joinArea xs (y:ys)
    | otherwise = y : joinArea (x:xs) ys

meetArea :: Ord a => RangeArea a -> RangeArea a -> RangeArea a
meetArea [] _ = []
meetArea _ [] = []
meetArea (x:xs) (y:ys)
    | intersect x y = meetRange x y : meetArea xs' ys'
    | otherwise = meetArea xs' ys'
    where
        (xs', ys')
            | rightEnd x <= rightEnd y = (xs, y:ys)
            | otherwise = (x:xs, ys)

generateArea :: (Variate a, Num a, Ord a, PrimMonad f) => Gen (PrimState f) -> (a -> a) -> RangeArea a -> f (Maybe a)
generateArea st eps [] = pure Nothing
generateArea st eps xs = do
    let n = length xs
    x <- (xs !!) <$> uniformR (0, n-1) st
    let leftBound = case leftEnd x of
            LeftEnd Open          -> Nothing
            LeftEnd (Inclusive a) -> pure a
            LeftEnd (Exclusive a) -> pure $ a + eps a
        rightBound = case rightEnd x of
            RightEnd Open          -> Nothing
            RightEnd (Inclusive a) -> pure a
            RightEnd (Exclusive a) -> pure $ a - eps a
    case (leftBound, rightBound) of
        (Nothing, Nothing) -> Just <$> uniformR (-100, 100) st
        (Just a, Nothing)  -> Just <$> uniformR (a , a + 100) st
        (Nothing, Just a)  -> Just <$> uniformR (a - 100, a) st
        (Just a, Just b)
            | nullRange (Range (LeftEnd $ Inclusive a) (RightEnd $ Inclusive b))  -> pure Nothing
            | otherwise -> Just <$> uniformR (a, b) st
