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
    go (String txt) tyExp | Prim PString <- typeExpName tyExp =
        matchString txt (typeExpRef tyExp)
    go (Number num) tyExp | Prim PNumber <- typeExpName tyExp =
        matchNumber num (typeExpRef tyExp)
    go (Bool bool) tyExp | Prim PBool <- typeExpName tyExp =
        matchBool bool (typeExpRef tyExp)
    go Null tyExp | Prim PNull <- typeExpName tyExp = pure ()
    go (Object obj) tyExp | Prim PObject <- typeExpName tyExp =
        matchObject obj (typeExpExt tyExp) (typeExpRef tyExp)
    go val ty = throwError $ show val ++ " does not match type: " ++ show ty
    matchString :: Text -> TypeRefine -> Except String ()
    matchString txt refs = pure () -- to be implemented
    matchNumber :: Scientific -> TypeRefine -> Except String ()
    matchNumber _ _ = pure () -- to be implemented
    matchBool :: Bool -> TypeRefine -> Except String ()
    matchBool _ _ = pure ()
    matchObject :: Object -> TypeExtension -> TypeRefine -> Except String ()
    matchObject _ _ _ = pure () -- to be implemented

