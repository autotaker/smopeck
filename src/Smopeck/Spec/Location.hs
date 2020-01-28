{-# LANGUAGE DeriveFunctor #-}
module Smopeck.Spec.Location where
import           Smopeck.Spec.Syntax

data LocationF a =
    Root String
    | Field (LocationF a) FieldName
    | Get (LocationF a) a
    deriving (Eq, Ord, Show, Functor)

data Blob = BlobAny | BlobInt Int
type LocationBlob = LocationF Blob
type Location = LocationF Int

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

