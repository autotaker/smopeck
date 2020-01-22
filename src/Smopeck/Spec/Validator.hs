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
import           Smopeck.Spec.Syntax

type Env = M.Map VarName Value
validateJson :: TypeEnv -> Env -> Value -> TypeExp -> Except String ()
validateJson tyEnv env = go
    where
    go :: Value -> TypeExp -> Except String ()
    go val (UnionType ty1 ty2) = go val ty1 <|> go val ty2
    go val (IntersectionType ty1 ty2) = go val ty1 >> go val ty2
    go (String txt) (TypeExp (Prim PString) _ tyRef) = matchString txt tyRef
    go (Number num) (TypeExp (Prim PNumber) _ tyRef) = matchNumber num tyRef
    go (Bool bool) (TypeExp (Prim PBool) _ tyRef) = matchBool bool tyRef
    go Null (TypeExp (Prim PNull) _ _) = pure ()
    go (Object obj) (TypeExp (Prim PObject) ext tyRef) = matchObject obj ext tyRef
    go val ty = throwError $ show val ++ " does not match type: " ++ show ty
    matchString :: Text -> TypeRefine -> Except String ()
    matchString txt refs = pure () -- to be implemented
    matchNumber :: Scientific -> TypeRefine -> Except String ()
    matchNumber _ _ = pure () -- to be implemented
    matchBool :: Bool -> TypeRefine -> Except String ()
    matchBool _ _ = pure ()
    matchObject :: Object -> TypeExtension -> TypeRefine -> Except String ()
    matchObject _ _ _ = pure () -- to be implemented

