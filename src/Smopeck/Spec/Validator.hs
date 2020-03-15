{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
module Smopeck.Spec.Validator where

import           Control.Applicative
import           Control.Monad.Except
import qualified Data.Map              as M
import           Data.Scientific
import qualified Data.Set              as S
import           Smopeck.Mock.Location
import           Smopeck.Mock.Value
import           Smopeck.Spec.Exp
import           Smopeck.Spec.Lattice
import           Smopeck.Spec.TypeExp


validate :: WHNFTypeEnv Desugar -> M.Map ALocation Value -> ALocation -> TypeExp Desugar WHNF -> Except String ()
validate tyEnv env = goLattice
    where
    goLattice :: ALocation -> TypeExp Desugar WHNF -> Except String ()
    goLattice it = cata CataJoin{
        fJBot = throwError "bottom type",
        fJJoin = (<|>),
        fJElem = go it
    }
    go :: ALocation -> TypeExpF Desugar WHNF -> Except String ()
    go it TypeExpF{..} =
        let value = env M.! it in
        case (typeExpName, value) of
            (Prim PNull, VNull)       -> pure ()
            (Prim PNumber, VNumber n) -> goRefs it (LNumber n) typeExpRef
            (Prim PInt, VNumber n)    -> goRefs it (LNumber n) typeExpRef
            (Prim PString, VString s) -> goRefs it (LString s) typeExpRef
            (Prim PBool, VBool b)     -> goRefs it (LBool b) typeExpRef
            (Prim PObject, VObject fields) ->
                forM_ (M.assocs typeExpExt) $ \(FieldString field, ty) -> do
                    unless (field `S.member` fields) $
                        throwError $ "field " ++ show field ++ " is not member of " ++ show it
                    goLattice (it `Field` field) (evalTypeExp tyEnv ty)
            (Prim PArray, VArray) -> do
                let tyLen = typeExpExt M.! FieldString "length"
                let VNumber l = env M.! (it `Field` "length")
                goLattice (it `Field` "length") (evalTypeExp tyEnv tyLen)
                forM_ [0..floor l - 1] $ \i -> do
                    let tyElt = typeExpExt M.! FieldIndex BindDebrujin
                    goLattice (it `Get` i) (evalTypeExp tyEnv tyElt)

    goRefs :: ALocation -> Literal Desugar -> TypeRefine Desugar -> Except String ()
    goRefs it lhs = mapM_ $ \(op, e) ->
        let rhs = evalExp env it e in
        unless (interpret op [lhs, rhs] == LBool True) $
            throwError $
                "expected " ++ show it ++ show op ++ show e ++ "\n"
                ++ "but found" ++ show lhs ++ show op ++ show rhs


evalExp :: M.Map ALocation Value -> ALocation -> Exp Desugar -> Literal Desugar
evalExp env base (Exp e) = go e
    where
    go (Literal l) = l
    go (Var x)     =
        case resolve base (toLocation x) of
            Left loc -> toLiteral $ env M.! loc
            Right i  -> LNumber (fromIntegral i)
    go (App op args) = interpret op (map go args)
    toLocation = fmap $ \e ->
        case evalExp env base e of
            LNumber i -> floor i
            v         -> error $ "index expected but found: " ++  show v
toLiteral :: Value -> Literal Desugar
toLiteral (VBool b)   = LBool b
toLiteral (VNumber n) = LNumber n
toLiteral (VString s) = LString s
toLiteral VNull       = LNull
toLiteral VArray      = error "array is not a literal"
toLiteral (VObject _) = error "object is not a literal"


