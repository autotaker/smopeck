{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
module Smopeck.Spec.Validator where

import           Control.Applicative
import           Control.Monad.Except
import qualified Data.Aeson            as A
import qualified Data.HashMap.Strict   as HM
import qualified Data.Map              as M
import           Data.Scientific
import qualified Data.Set              as S
import qualified Data.Text             as T
import qualified Data.Vector           as V
import           Smopeck.Mock.Location
import           Smopeck.Mock.Value
import           Smopeck.Spec.Exp
import           Smopeck.Spec.Lattice
import           Smopeck.Spec.TypeExp

validateJson :: WHNFTypeEnv Desugar -> M.Map String A.Value
                -> String -> TypeExp Desugar WHNF -> Except String ()
validateJson tyEnv env x = validate tyEnv env' loc
    where
    loc = Root (Absolute x)
    env' = M.fromList $ M.assocs env >>= unfold
    unfold :: (String, A.Value) -> [ (ALocation, Value )]
    unfold (root, val) = go (Root (Absolute root)) val
    go it v = case v of
        A.Null -> pure (it, VNull)
        A.Bool b -> pure (it, VBool b)
        A.Number n -> pure (it, VNumber n)
        A.String s -> pure (it, VString (T.unpack s))
        A.Object obj ->
            pure (it, VObject fields) <>
            (HM.toList obj >>= \(key, val) -> go (it `Field` T.unpack key) val)
            where
            fields = S.fromList $ map T.unpack $ HM.keys obj
        A.Array arr -> pure (it, VArray) <>
            pure (it `Field` "length", VNumber (fromIntegral n)) <>
            ([0..n-1] >>= (\i -> go (it `Get` i) (arr V.! i)))
            where n = length arr

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
            (Prim ty, _) -> throwError $ "value " ++ show value ++ " is not " ++ show ty

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


