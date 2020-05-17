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
    | Chain (LocationF root a) (Field a)
    deriving (Eq, Ord, Show, Functor, Generic, Foldable, Traversable)

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

data Field a = FieldString String | FieldIndex a
    deriving(Eq,Ord,Show, Functor, Generic, Foldable, Traversable)
type RLocationF a = LocationF () a

pattern Field p f = Chain p (FieldString f)
pattern Get p i = Chain p (FieldIndex i)



resolve :: ALocation -> Location -> Either ALocation Int
resolve base = go
    where
        go (Root (Absolute s)) = Left $ Root (Absolute s)
        go (Root (Relative i)) = sub base i
        go (Chain p f)         = case go p of
            Left l -> Left $ Chain l f
            Right _ -> error $ "Field Access for Index:" ++  show (base, p, f)
        sub loc 0         = Left loc
        sub (Field p f) i = sub p (i-1)
        sub (Get p i) 1   = Right i
        sub (Get p _) i   = sub p (i-2)

match :: LocationBlob -> Location -> Bool
match (Root s1) (Root s2) = s1 == s2
match (Root _) _ = False
match (Field parent1 field1) (Field parent2 field2) =
    field1 == field2 &&  match parent1 parent2
match (Field _ _) _ = False
match (Get parent1 BlobAny) (Get parent2 _) = match parent1 parent2
match (Get parent1 (BlobInt i)) (Get parent2 j) =
    i == j && match parent1 parent2
match (Get _ _) _ = False

castRoot :: LocationF (Root RootAbs) a -> LocationF (Root m) a
castRoot = unsafeCoerce


mapRoot :: (root -> root') -> LocationF root a -> LocationF root' a
mapRoot f = go
  where
    go (Root root) = Root (f root)
    go (Chain p f) = go p `Chain` f