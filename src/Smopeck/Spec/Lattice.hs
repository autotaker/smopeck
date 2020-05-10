{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Smopeck.Spec.Lattice where
import           Control.Monad
import           Control.Monad.Free
import           Data.Function
import           Data.Kind
import           Data.Void
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

data LatticeExt (m :: LatticeMode) f a where
    LBot  :: JoinSupported m => LatticeExt m f a
    LElem :: a -> LatticeExt m f a
    LJoin :: JoinSupported m => LatticeExt m f a -> LatticeExt m f a -> LatticeExt m f a
    LMeet :: MeetSupported m => LatticeExt m f a -> LatticeExt m f a -> LatticeExt m f a
    LTop  :: MeetSupported m => LatticeExt m f a
    LExt  :: f (LatticeExt m f a) -> LatticeExt m f a

newtype VoidF a = VoidF Void
    deriving(Functor, Foldable, Traversable, Show, Eq, Ord)
type Lattice m = LatticeExt m VoidF


fmapWithExt :: Functor f => (a -> b) -> (forall a. f a -> g a) -> LatticeExt m f a -> LatticeExt m g b
fmapWithExt f h = go
    where
    go LBot        = LBot
    go LTop        = LTop
    go (LElem a)   = LElem $ f a
    go (LJoin a b) = LJoin (go a) (go b)
    go (LMeet a b) = LMeet (go a) (go b)
    go (LExt ta)   = LExt $ h $ fmap go ta

deriving instance Functor f => Functor (LatticeExt m f)
deriving instance Foldable f => Foldable (LatticeExt m f)
deriving instance Traversable f => Traversable (LatticeExt m f)
deriving instance Show a => Show (Lattice m a)
deriving instance Eq a => Eq (Lattice m a)
deriving instance Ord a => Ord (Lattice m a)

instance Functor f => Applicative (LatticeExt m f) where
    pure = LElem
    (<*>) = ap

instance Functor  f => Monad (LatticeExt m f) where
    LBot >>= _ = LBot
    LTop >>= _ = LTop
    LElem a >>= f = f a
    LJoin a b >>= f = LJoin (a >>= f) (b >>= f)
    LMeet a b >>= f = LMeet (a >>= f) (b >>= f)
    LExt ta >>= f = LExt (fmap (>>= f) ta)
    -- ta :: f (LatticeExt m f a)
    -- f :: a -> LatticeExt m f b
    -- fmap (>>= f) ta ::  f (LatticeExt m f b))

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

instance Cata (Lattice Full) where
    data CataF (Lattice Full) s a = CataFull {
        fFTop :: s,
        fFBot :: s,
        fFElem :: a -> s,
        fFMeet :: s -> s -> s,
        fFJoin :: s -> s -> s
    }
    cata g = go
        where
            go LTop        = fFTop g
            go LBot        = fFBot g
            go (LElem a)   = fFElem g a
            go (LMeet a b) = fFMeet g (go a) (go b)
            go (LJoin a b) = fFJoin g (go a) (go b)


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

toJoinExtNormalForm :: Traversable f => LatticeExt m f a -> Lattice Join (Lattice Meet (Free f a))
toJoinExtNormalForm = fix $ \go -> \case
    LBot      -> LBot
    LTop      -> LElem LTop
    LElem a -> LElem (LElem (pure a))
    LJoin a b -> LJoin (go a) (go b)
    LMeet a b -> LMeet <$> go a <*> go b
    LExt fa -> fmap Free . sequenceA <$> traverse go fa

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

castFull :: Lattice m a -> LatticeExt Full f a
castFull = unsafeCoerce
