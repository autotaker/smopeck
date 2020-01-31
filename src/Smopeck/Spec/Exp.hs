{-# LANGUAGE DeriveFunctor #-}
module Smopeck.Spec.Exp where
import           Control.Monad
import qualified Data.Map        as M
import           Data.Scientific

data ExpF a = Literal Literal | Var a | App Op [ExpF a]
    deriving (Eq, Ord, Show, Functor)

instance Applicative ExpF where
    pure = Var
    (<*>) = ap

instance Monad ExpF where
    Literal l >>= f = Literal l
    (Var x) >>= f = f x
    (App op args) >>= f = App op (map (>>= f) args)

data Op = Add | Sub | Mul | Div
        | Eq | Lt | Gt | Lte | Gte | Match
        deriving(Eq,Ord, Show)

data Literal =
    LNull
    | LBool Bool
    | LNumber Scientific
    | LString String
    | LRegex String

eval :: (Eq a, Ord a, Show a) =>M.Map a Literal -> Exp a -> Literal
eval env (Literal l) = l
eval env (Var x) = case M.lookup x env of
    Nothing -> error $ "undefined variable: " ++ show x
    Just l  -> l
eval env (App op args) = interpret op $ map (eval env) args

interpret :: Op -> [Literal] -> Literal
interpret Add [LNumber x, LNumber y] = LNumber $ x + y
interpret Add [LString x, LString y] = LString $ x ++ y
interpret Add [LRegex x, LRegex y] = LRegex $ x ++ y
interpret Sub [LNumber x, LNumber y] = LNumber $ x - y
interpret Mul [LNumber x, LNumber y] = LNumber $ x * y
interpret Div [LNumber x, LNumber y] = LNumber $ x / y
interpret Eq [x, y] = LBool $ x == y
interpret Lt [x, y] = LBool $ x < y
interpret Gt [x, y] = LBool $ x > y
interpret Lte [x, y] = LBool $ x <= y
interpret Gte [x, y] = LBool $ x >= y
interpret Match [x, LRegex s] = LBool $ undefined
interpret op args =
    error $ "no interpretation for :" ++ show op ++ " " ++ show args