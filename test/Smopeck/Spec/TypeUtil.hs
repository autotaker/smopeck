{-# LANGUAGE DataKinds #-}
module Smopeck.Spec.TypeUtil where

import qualified Data.Map              as M
import           Smopeck.Mock.Location
import           Smopeck.Spec.Exp
import           Smopeck.Spec.Lattice
import           Smopeck.Spec.TypeExp

fPrim :: Primitive -> TypeExtension 'Desugar -> TypeRefine 'Desugar -> TypeExp 'Desugar head
fPrim prim ext ref = LElem (TypeExpF (Prim prim) BindDebrujin  ext ref NoCond)

fNull :: TypeExp 'Desugar head
fNull = fPrim PNull M.empty []

fInt, fBool, fNumber, fString :: TypeRefine 'Desugar -> TypeExp 'Desugar head
fInt = fPrim PInt M.empty
fBool = fPrim PBool M.empty
fNumber = fPrim PNumber M.empty
fString = fPrim PString M.empty
fObject :: TypeExtension 'Desugar -> TypeExp 'Desugar head
fObject ext = fPrim PObject ext []
fArray :: TypeExp 'Desugar 'HDefault -> TypeExp 'Desugar 'HDefault -> TypeExp 'Desugar head
fArray size elt = fPrim PArray (M.fromList [
    (FieldString "length", size),
    (FieldIndex BindDebrujin , elt)]) []

fCond :: Exp 'Desugar -> TypeExp 'Desugar 'HDefault -> TypeExp 'Desugar 'HDefault
fCond e ty = LExt $ HasCondF e ty

withCond :: TypeExp mode 'WHNF -> Exp mode -> TypeExp mode 'WHNF
withCond ty e = fmap (\tyf -> tyf{ typeExpCond = HasCond e}) ty
