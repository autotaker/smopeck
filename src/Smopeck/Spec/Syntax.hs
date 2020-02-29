{-# LANGUAGE DataKinds #-}
module Smopeck.Spec.Syntax(
    TopLevelDef(..),
    Route, Method, VarName, UserType, TypeExp, TypeEnv, Primitive,
    TypeExtension, Exp, TypeRefine, FieldName,
    module Smopeck.Spec.Exp,
    fTypeExp
)  where

import qualified Data.Map              as M
import           Smopeck.Mock.Location
import           Smopeck.Spec.Exp
import           Smopeck.Spec.Lattice
import qualified Smopeck.Spec.TypeExp  as TypeExp
import           Text.Read             hiding (Number, String)

data TopLevelDef =
    TypeDef UserType TypeExp
    | EndpointDef Route Method TypeExtension
    deriving(Eq, Show)

type Route = TypeExp.Route
type Method = String
type VarName = TypeExp.VarName
type UserType = TypeExp.UserType
type TypeExp = TypeExp.TypeExp Parsed TypeExp.HDefault


type TypeEnv = TypeExp.DefaultTypeEnv Parsed
type Primitive = TypeExp.Primitive

type TypeExtension = [(UserType, TypeExp) ]
type Exp = TypeExp.Exp Parsed
type TypeRefine = TypeExp.TypeRefine Parsed

fTypeExp :: String -> String -> TypeExtension -> TypeRefine -> TypeExp
fTypeExp tyName bindName ext ref =
    LElem $ TypeExp.TypeExpF
        (read tyName)
        (TypeExp.BindName bindName)
        (M.fromList ext)
        ref
