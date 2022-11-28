{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE StandaloneDeriving #-}
module Smopeck.Mock.Location where

import           Data.Hashable
import           GHC.Generics
import           Unsafe.Coerce


data LocationF root a =
    Root root
    | Chain (LocationF root a) (Field a) Optionality
    deriving (Eq, Ord, Show, Functor, Generic, Foldable, Traversable)

data Optionality = Optional | Mandatory
    deriving (Eq, Ord, Show, Generic)

instance Hashable Optionality

instance (Hashable root, Hashable a) => Hashable (LocationF root a)
instance Hashable (Root m) where
    hashWithSalt salt (Relative a) = salt `hashWithSalt` (1 :: Int) `hashWithSalt` a
    hashWithSalt salt (Absolute a) = salt `hashWithSalt` (2 :: Int) `hashWithSalt` a
instance Hashable a => Hashable (Field a)

data RootMode = RootAbs | RootAny

data Blob = BlobAny | BlobInt Int
    deriving(Eq, Ord, Show)
type LocationBlob = LocationF (Root RootAny) Blob
type Location = LocationF (Root RootAny) Int
type ALocation = LocationF (Root RootAbs) Int

data Root (m :: RootMode) where
    Relative :: Int -> Root RootAny -- De Brujin Index
    Absolute :: String -> Root m

deriving instance (Eq (Root m))
deriving instance (Ord (Root m))
deriving instance (Show (Root m))
type FieldName = String

data Field a
  = FieldString String -- ^ a field of object
  | FieldIndex a  -- ^ an element of array
  deriving(Eq,Ord,Show, Functor, Generic, Foldable, Traversable)
type RLocationF a = LocationF () a

pattern Field p f opt = Chain p (FieldString f) opt
pattern Get p i = Chain p (FieldIndex i) Mandatory


-- | resolve a relative location to absolute from a base location
--
-- Return Value:
--   - @Left loc@ if location resolved to a location
--   - @Right n@ if location resolved to an index of some array
--
-- Examples:
--
-- @
--   resolve fuga.get(3).piyo 1 => Left fuga.get(3)
--   resolve fuga.get(3).piyo 2 => Right 3
--   resolve fuga.get(3).piyo 1.bar => Left fuga.get(3).bar
--   resolve fuga.get(3).piyo bar => Left bar
-- @
--
resolve :: ALocation -> Location -> Either ALocation Int
resolve base = go
    where
        go (Root (Absolute s)) = Left $ Root (Absolute s)
        go (Root (Relative i)) = sub base i
        go (Chain p f opt)         = case go p of
            Left l  -> Left $ Chain l f opt
            Right _ -> error $ "Field Access for Index:" ++  show (base, p, f)
        sub loc 0           = Left loc
        sub (Field p f _) i = sub p (i-1)
        sub (Get p i) 1     = Right i
        sub (Get p _) i     = sub p (i-2)

match :: LocationBlob -> Location -> Bool
match (Root s1) (Root s2) = s1 == s2
match (Root _) _ = False
match (Field parent1 field1 _) (Field parent2 field2 _) =
    field1 == field2 &&  match parent1 parent2
match (Field {}) _ = False
match (Get parent1 BlobAny) (Get parent2 _) = match parent1 parent2
match (Get parent1 (BlobInt i)) (Get parent2 j) =
    i == j && match parent1 parent2
match (Get _ _) _ = False

castRoot :: LocationF (Root RootAbs) a -> LocationF (Root m) a
castRoot = unsafeCoerce


mapRoot :: (root -> root') -> LocationF root a -> LocationF root' a
mapRoot f = go
  where
    go (Root root)     = Root (f root)
    go (Chain p f opt) = Chain (go p) f opt
