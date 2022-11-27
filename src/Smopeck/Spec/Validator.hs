{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}

module Smopeck.Spec.Validator where

import           Control.Applicative
import           Control.Monad.Except
import qualified Data.Aeson            as A
import qualified Data.Aeson.Key        as K
import qualified Data.Aeson.KeyMap     as KM
import           Data.Char
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
import           Text.Read

validateJson ::
  WHNFTypeEnv Desugar ->
  M.Map String A.Value ->
  String ->
  TypeExp Desugar WHNF ->
  Either String ()
validateJson tyEnv env x = runExcept . validate tyEnv env' loc
  where
    loc = Root (Absolute x)
    env' = M.fromList $ M.assocs env >>= unfold
    -- unfold objects to mappings from locations to primitives
    unfold :: (String, A.Value) -> [(ALocation, Value)]
    unfold (root, val) = go (Root (Absolute root)) val
    -- it: current location
    -- v: value to unfold
    go it v = case v of
      A.Null -> pure (it, VNull)
      A.Bool b -> pure (it, VBool b)
      A.Number n -> pure (it, VNumber n)
      A.String s -> pure (it, VString (T.unpack s))
      A.Object obj ->
        pure (it, VObject fields)
          <> (KM.toList obj >>= \(key, val) -> go (Field it (K.toString key) Mandatory) val)
        where
          fields = S.fromList $ map K.toString $ KM.keys obj
      A.Array arr ->
        pure (it, VArray)
          <> pure (Field it "length" Mandatory, VNumber (fromIntegral n))
          <> ([0 .. n - 1] >>= (\i -> go (it `Get` i) (arr V.! i)))
        where
          n = length arr

validate :: WHNFTypeEnv Desugar -> M.Map ALocation Value -> ALocation -> TypeExp Desugar WHNF -> Except String ()
validate tyEnv env = goLattice
  where
    goLattice :: ALocation -> TypeExp Desugar WHNF -> Except String ()
    goLattice it =
      cata
        CataJoin
          { fJBot = throwError "bottom type",
            fJJoin = (<|>),
            fJElem = go it
          }
    go :: ALocation -> TypeExpF Desugar WHNF -> Except String ()
    go it TypeExpF {..} = do
      let value = env M.! it
          checkCond (HasCond e) = evalExp env it e == LBool True
          checkCond NoCond      = True
      unless (checkCond typeExpCond) $ throwError "type cond does not hold"
      case (typeExpName, value) of
        (Prim PNull, VNull) -> pure ()
        (Prim PNumber, VNumber n) -> goRefs it (LNumber n) typeExpRef
        (Prim PInt, VNumber n) -> goRefs it (LNumber n) typeExpRef
        (Prim PString, VString s) -> goRefs it (LString s) typeExpRef
        (Prim PBool, VBool b) -> goRefs it (LBool b) typeExpRef
        (Prim PObject, VObject fields) ->
          forM_ (M.assocs typeExpExt) $ \(FieldString field, (ty, opt)) ->
            case (opt, field `S.member` fields) of
              (Optional, False) -> pure ()
              (Mandatory, False) -> throwError $
                "field " ++ show field ++ " is not member of " ++ show it
              (_, True) ->
                goLattice (Field it field Mandatory) (evalTypeExp tyEnv ty)
        (Prim PArray, VArray) -> do
          let tyLen = fst $ typeExpExt M.! FieldString "length"
          let VNumber l = env M.! Field it "length" Mandatory
          goLattice (Field it "length" Mandatory) (evalTypeExp tyEnv tyLen)
          forM_ [0 .. floor l - 1] $ \i -> do
            let tyElt = fst $ typeExpExt M.! FieldIndex BindDebrujin
            goLattice (it `Get` i) (evalTypeExp tyEnv tyElt)
        (Prim ty, _) -> throwError $ "value " ++ show value ++ " is not " ++ show ty

    goRefs :: ALocation -> Literal Desugar -> TypeRefine Desugar -> Except String ()
    goRefs it lhs = mapM_ $ \(op, e) ->
      let rhs = evalExp env it e
       in unless (interpret op [lhs, rhs] == LBool True) $
            throwError $
              "expected "
                ++ show it
                ++ show op
                ++ show e
                ++ "\n"
                ++ "but found"
                ++ show lhs
                ++ show op
                ++ show rhs

parseParam :: String -> TypeExp Desugar WHNF -> Except String A.Value
parseParam val =
  cata
    CataJoin
      { fJBot = throwError "void type",
        fJElem = go,
        fJJoin = (<|>)
      }
  where
    go :: TypeExpF Desugar WHNF -> Except String A.Value
    go TypeExpF {..} =
      case typeExpName of
        Prim PInt -> do
          n <- ExceptT $ pure $ fmap fromInteger $ readEither val
          goRefs (LNumber n) typeExpRef
          pure $ A.Number n
        Prim PNumber -> do
          n <- ExceptT $ pure $ readEither val
          goRefs (LNumber n) typeExpRef
          pure $ A.Number n
        Prim PString -> do
          goRefs (LString val) typeExpRef
          pure $ A.String $ T.pack val
        Prim PBool -> do
          b <- case map toLower val of
            "true"  -> pure True
            "false" -> pure False
            _       -> throwError $ "not Boolean: " ++ val
          goRefs (LBool b) typeExpRef
          pure $ A.Bool b
        Prim PNull -> do
          case map toLower val of
            "null" -> pure ()
            ""     -> pure ()
            _      -> throwError $ "not Null: " ++ val
          pure $ A.Null
        Prim PArray -> error "Array is not primitive"
        Prim PObject -> error "Object is not primitive"

    goRefs :: Literal Desugar -> TypeRefine Desugar -> Except String ()
    goRefs lhs = mapM_ $ \(op, e) -> do
      let rhs = evalExp M.empty (Root (Absolute "")) e
      unless (interpret op [lhs, rhs] == LBool True) $
        throwError $
          "not match " ++ show (lhs, op, rhs)

evalExp :: M.Map ALocation Value -> ALocation -> Exp Desugar -> Literal Desugar
evalExp env base (Exp e) = go e
  where
    go (Literal l) = l
    go (Var x) =
      case resolve base (toLocation x) of
        Left loc -> toLiteral $ env M.! loc
        Right i  -> LNumber (fromIntegral i)
    go (App op args) = interpret op (map go args)
    toLocation = fmap $ \e ->
      case evalExp env base e of
        LNumber i -> floor i
        v         -> error $ "index expected but found: " ++ show v

toLiteral :: Value -> Literal Desugar
toLiteral (VBool b)   = LBool b
toLiteral (VNumber n) = LNumber n
toLiteral (VString s) = LString s
toLiteral VNull       = LNull
toLiteral VArray      = error "array is not a literal"
toLiteral (VObject _) = error "object is not a literal"
