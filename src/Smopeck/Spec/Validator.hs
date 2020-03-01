{-# LANGUAGE DataKinds #-}
module Smopeck.Spec.Validator(
    validateJson
    , Env
) where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Aeson
import qualified Data.Map             as M
import           Data.Scientific
import           Data.Text
import           Smopeck.Spec.Lattice
import           Smopeck.Spec.Syntax
import           Smopeck.Spec.TypeExp (BindName, Primitive (..), TypeName (..),
                                       typeExpExt, typeExpName, typeExpRef)

type Env = M.Map VarName Value
validateJson :: TypeEnv -> Env -> Value -> TypeExp -> Except String ()
validateJson tyEnv env = go
    where
    go :: Value -> TypeExp -> Except String ()
    go val (LJoin ty1 ty2) = go val ty1 <|> go val ty2
    go val (LMeet ty1 ty2) = go val ty1 >> go val ty2
    go (String txt) (LElem tyExp) | Prim PString <- typeExpName tyExp =
        matchString txt (typeExpRef tyExp)
    go (Number num) (LElem tyExp) | Prim PNumber <- typeExpName tyExp =
        matchNumber num (typeExpRef tyExp)
    go (Bool bool) (LElem tyExp) | Prim PBool <- typeExpName tyExp =
        matchBool bool (typeExpRef tyExp)
    go Null (LElem tyExp) | Prim PNull <- typeExpName tyExp = pure ()
    go (Object obj) (LElem tyExp) | Prim PObject <- typeExpName tyExp =
        matchObject obj (typeExpExt tyExp) (typeExpRef tyExp)
    go val ty = throwError $ show val ++ " does not match type: " ++ show ty
    matchString :: Text -> TypeRefine -> Except String ()
    matchString txt refs = pure () -- to be implemented
    matchNumber :: Scientific -> TypeRefine -> Except String ()
    matchNumber _ _ = pure () -- to be implemented
    matchBool :: Bool -> TypeRefine -> Except String ()
    matchBool _ _ = pure ()
    matchObject :: Object -> M.Map (Field (BindName Parsed)) TypeExp -> TypeRefine -> Except String ()
    matchObject _ _ _ = pure () -- to be implemented

