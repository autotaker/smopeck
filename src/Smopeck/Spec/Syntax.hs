module Smopeck.Spec.Syntax  where

data TopLevelDef =
    TypeDef TypeName TypeExp
    | EndpointDef Route Method TypeExtension
    deriving(Eq, Ord, Show)

type TypeName = String
type Route = String
type Method = String
type FieldName = String
type VarName = String

data TypeExp =
    TypeExp {
        typeExpName :: TypeName
      , typeExpExt  :: TypeExtension
      , typeExpRef  :: TypeRefine
    }
    | UnionType TypeExp TypeExp
    | IntersectionType TypeExp TypeExp
    deriving(Eq, Ord, Show)
type TypeExtension = [ (FieldName, TypeExp) ]
type TypeRefine = [ RefineExp ]

data RefineExp =
    RefineAtom {
        refineAtomVar        :: VarName,
        refineAtomFieldChain :: [FieldAccessor]
      }
    | RefineLiteral Literal
    | RefineBin BinOp RefineExp RefineExp
    deriving(Eq, Ord,Show)
data FieldAccessor =
    FieldAccessor {
        fieldTypeName :: Maybe TypeName
      , fieldName     :: FieldName
    } deriving(Eq, Ord,Show)

data Literal =
    DQStringLiteral String
    | SQStringLiteral String
    | BooleanLiteral Bool
    | NumberLiteral Double
    | RegexLiteral String
    deriving(Eq, Ord,Show)

data BinOp = OpEq | OpMatch deriving(Eq, Ord,Show)
