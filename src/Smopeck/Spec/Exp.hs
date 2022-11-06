{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
module Smopeck.Spec.Exp(
    eval,
    locations,
    interpret,
    ExpF(..),
    Literal(..),
    Mode(..),
    Op(..)) where
import           Control.Monad
import           Data.Coerce
import qualified Data.Map               as M
import           Data.Scientific
import qualified Data.Set               as S
import           Smopeck.Spec.RegexUtil

data ExpF (mode :: Mode) a = Literal !(Literal mode) | Var !a | App !Op [ExpF mode a]
    deriving (Functor)

deriving instance Eq a => (Eq (ExpF Parsed a))
deriving instance Ord a => (Ord (ExpF Parsed a))
deriving instance Show a => (Show (ExpF Parsed a))

deriving instance Eq a => (Eq (ExpF Desugar a))
deriving instance Ord a => (Ord (ExpF Desugar a))
deriving instance Show a => (Show (ExpF Desugar a))

data Mode = Parsed | Desugar

instance Applicative (ExpF mode) where
    pure = Var
    (<*>) = ap

instance Monad (ExpF mode) where
    Literal l >>= f     = Literal l
    (Var x) >>= f       = f x
    (App op args) >>= f = App op (map (>>= f) args)

data Op = Add | Sub | Mul | Div | Mod
        | Eq | Lt | Gt | Lte | Gte | Match
        | And | Or | Func String
        deriving(Eq,Ord, Show)

data Literal (mode :: Mode) where
    LNull :: Literal mode
    LBool  :: !Bool -> Literal mode
    LNumber :: !Scientific -> Literal mode
    LString :: !String -> Literal mode
    LDQString :: !String -> Literal Parsed
    LRegex :: !String -> Literal mode

deriving instance (Show (Literal Parsed))
deriving instance (Eq (Literal Parsed))
deriving instance (Ord (Literal Parsed))
deriving instance (Show (Literal Desugar))
deriving instance (Eq (Literal Desugar))
deriving instance (Ord (Literal Desugar))

eval :: (Eq a, Ord a, Show a) =>M.Map a (Literal Desugar) -> ExpF Desugar a -> Either a (Literal Desugar)
eval env = go
    where
    go (Literal l)   = Right l
    go (Var x)       = Left x
    go (App op args) = Right $ interpret op $ map (deref . go) args

    deref (Left x) = case M.lookup x env of
        Just l  -> l
        Nothing -> error $ "failed to dereference:" ++ show x
    deref (Right l) = l

locations :: (Eq a, Ord a, Show a) => ExpF m a -> S.Set a
locations (Literal _)  = S.empty
locations (Var x)      = S.singleton  x
locations (App _ args) = S.unions (map locations args)

interpret :: Op -> [Literal Desugar] -> Literal Desugar
interpret Add l = foldl1 f l
    where
    f (LNumber x) (LNumber y) = LNumber $ x + y
    f (LString x) (LString y) = LString $ x ++ y
    f (LRegex x) (LRegex y)   = LRegex $ x ++ y
    f (LString x) (LRegex y)  = LRegex $ coerce (escapeR x) ++ y
    f (LRegex x) (LString y)  = LRegex $ x ++ coerce (escapeR y)
interpret Sub [LNumber x, LNumber y] = LNumber $ x - y
interpret Sub [LNumber x] = LNumber $ -x
interpret Mul [LNumber x, LNumber y] = LNumber $ x * y
interpret Div [LNumber x, LNumber y] = LNumber $ x / y
interpret Mod [LNumber x, LNumber y]
    | Just ix <- toInt x, Just iy <- toInt y = LNumber $ fromIntegral $ ix `mod` iy
    where toInt :: Scientific -> Maybe Int
          toInt = toBoundedInteger
interpret Eq [x, y] = LBool $ x == y
interpret Lt [x, y] = LBool $ x < y
interpret Gt [x, y] = LBool $ x > y
interpret Lte [x, y] = LBool $ x <= y
interpret Gte [x, y] = LBool $ x >= y
interpret Match [LString x, LRegex s] = LBool $ matchR x (RString s)
interpret And [LBool x, LBool y] = LBool $ x && y
interpret Or [LBool x, LBool y] = LBool $ x || y
interpret (Func "str") [x] =
    LString $ case x of
        LNumber x   -> show x
        LString x   -> x
        LRegex x    -> x
        LBool True  -> "true"
        LBool False -> "false"
        LNull       -> "null"
interpret op args =
    error $ "no interpretation for :" ++ show op ++ " " ++ show args
