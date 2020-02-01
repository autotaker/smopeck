{-# LANGUAGE DataKinds #-}
module Smopeck.Spec.Syntax  where

import qualified Data.Map              as M
import           Smopeck.Mock.Location
import           Smopeck.Spec.Exp
import           Text.Read             hiding (Number, String)

data TopLevelDef =
    TypeDef TypeName TypeExp
    | EndpointDef Route Method TypeExtension
    deriving(Eq, Ord, Show)

data TypeName = Prim Primitive | User String
    deriving(Eq,Ord,Show)
type Route = String
type Method = String
type VarName = String

type TypeEnv = M.Map TypeName TypeExp
data Primitive = PObject | PString | PNumber | PArray | PBool | PNull
    deriving(Eq,Ord,Show)

instance Read TypeName where
    readPrec = do
        Ident x <- lexP
        case x of
            "Object" -> pure $ Prim PObject
            "String" -> pure $ Prim PString
            "Number" -> pure $ Prim PNumber
            "Array"  -> pure $ Prim PArray
            "Bool"   -> pure $ Prim PBool
            "Null"   -> pure $ Prim PNull
            _        -> pure $ User x


data TypeExp =
    TypeExp {
        typeExpName :: TypeName,
        typeExpBind :: VarName,
        typeExpExt  :: TypeExtension,
        typeExpRef  :: TypeRefine
    }
    | UnionType TypeExp TypeExp
    | IntersectionType TypeExp TypeExp
    deriving(Eq, Ord, Show)
type TypeExtension = [ (FieldName, TypeExp) ]
newtype Exp = Exp (ExpF Parsed (LocationF Root Exp))
    deriving(Eq, Ord, Show)
type TypeRefine = [ (RLocationF Exp, Op, Exp) ]

data Literal =
    DQStringLiteral String
    | SQStringLiteral String
    | BooleanLiteral Bool
    | NumberLiteral Double
    | RegexLiteral String
    deriving(Eq, Ord,Show)

data BinOp = OpEq | OpMatch deriving(Eq, Ord,Show)
