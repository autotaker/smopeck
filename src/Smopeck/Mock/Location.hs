{-# LANGUAGE DeriveFunctor #-}
module Smopeck.Mock.Location where

data LocationF root a =
    Root root
    | Field (LocationF root a) FieldName
    | Get (LocationF root a) a
    deriving (Eq, Ord, Show, Functor)

data Blob = BlobAny | BlobInt Int
    deriving(Eq, Ord, Show)
type LocationBlob = LocationF Root Blob
type Location = LocationF Root Int

data Root = Relative | Absolute String
    deriving(Eq, Ord, Show)
type FieldName = String
type RLocationF a = LocationF () a

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

