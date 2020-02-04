{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE ExplicitForAll     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
module Smopeck.Spec.Lattice where
import           Control.Monad
import           Data.Kind
import           Unsafe.Coerce

data LatticeMode = Full | Join | Meet

type family MeetSupported (l :: LatticeMode) :: Constraint where
    MeetSupported Full = ()
    MeetSupported Meet = ()
    MeetSupported _ = (True ~ False)
type family JoinSupported (l :: LatticeMode) :: Constraint where
    JoinSupported Full = ()
    JoinSupported Join = ()
    JoinSupported _ = (True ~ False)

data Lattice (m :: LatticeMode) a where
    LBot  :: JoinSupported m => Lattice m a
    LElem :: a -> Lattice m a
    LJoin :: JoinSupported m => Lattice m a -> Lattice m a -> Lattice m a
    LMeet :: MeetSupported m => Lattice m a -> Lattice m a -> Lattice m a
    LTop  :: MeetSupported m => Lattice m a

deriving instance Functor (Lattice m)

instance Applicative (Lattice m) where
    pure = LElem
    (<*>) = ap

instance Monad (Lattice m) where
    LBot >>= _ = LBot
    LTop >>= _ = LTop
    LElem a >>= f = f a
    LJoin a b >>= f = LJoin (a >>= f) (b >>= f)
    LMeet a b >>= f = LMeet (a >>= f) (b >>= f)

class Cata f where
    data CataF f s a
    cata :: CataF f s a -> f a -> s

instance Cata (Lattice Join) where
    data CataF (Lattice Join) s a = CataJoin {
        fJBot :: s,
        fJElem :: a -> s,
        fJJoin :: s -> s -> s
    }
    cata g = go
        where
            go LBot        = fJBot g
            go (LElem a)   = fJElem g a
            go (LJoin a b) = fJJoin g (go a) (go b)

instance Cata (Lattice Meet) where
    data CataF (Lattice Meet) s a = CataMeet {
        fMTop :: s,
        fMElem :: a -> s,
        fMMeet :: s -> s -> s
    }
    cata g = go
        where
            go LTop        = fMTop g
            go (LElem a)   = fMElem g a
            go (LMeet a b) = fMMeet g (go a) (go b)

toJoinNormalForm :: Lattice m a -> Lattice Join (Lattice Meet a)
toJoinNormalForm LBot = LBot
toJoinNormalForm LTop = LElem LTop
toJoinNormalForm (LElem a) = LElem (LElem a)
toJoinNormalForm (LJoin a b) =
    LJoin (toJoinNormalForm a) (toJoinNormalForm b)
toJoinNormalForm (LMeet a b) = do
    ma <- toJoinNormalForm a
    mb <- toJoinNormalForm b
    pure $ LMeet ma mb

toMeetNormalForm :: Lattice m a -> Lattice Meet (Lattice Join a)
toMeetNormalForm LTop = LTop
toMeetNormalForm LBot = LElem LBot
toMeetNormalForm (LElem a) = LElem (LElem a)
toMeetNormalForm (LMeet a b) =
    LMeet (toMeetNormalForm a) (toMeetNormalForm b)
toMeetNormalForm (LJoin a b) = do
    ma <- toMeetNormalForm a
    mb <- toMeetNormalForm b
    pure $ LJoin ma mb

castFull :: Lattice m a -> Lattice Full a
castFull = unsafeCoerce
