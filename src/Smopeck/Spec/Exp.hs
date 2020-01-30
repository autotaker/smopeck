module Smopeck.Spec.Exp where
import           Data.Scientific

data ExpF a = Literal | Var a | App Op [ExpF a]

data Op = Add | Sub | Mul | Div
        | Eq | Lt | Gt | Lte | Gte

data Literal =
    LNull
    | LBool Bool
    | LNumber Scientific
    | LString String
    | LRegex String
