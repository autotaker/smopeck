{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
module Smopeck.Spec.TypeExp where

import qualified Data.Map              as M
import           Smopeck.Mock.Location
import           Smopeck.Spec.Exp
import           Text.Read             hiding (Number, String)

data TypeExp (mode :: Mode) =
    TypeExp {
        typeExpName :: TypeName,
        typeExpBind :: VarName,
        typeExpExt  :: TypeExtension mode,
        typeExpRef  :: TypeRefine mode
    }
    | UnionType (TypeExp mode) (TypeExp mode)
    | IntersectionType (TypeExp mode) (TypeExp mode)

data TypeName = Prim Primitive | User String
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

type Route = String
type VarName = String

type TypeEnv m = M.Map TypeName (TypeExp m)
data Primitive = PObject | PString | PNumber | PArray | PBool | PNull
    deriving(Eq,Ord,Show)
type TypeExtension mode = [ (FieldName, TypeExp mode)]

newtype Exp mode = Exp (ExpF mode (LocationF Root (Exp mode)))

type TypeRefine mode = [ (RLocationF (Exp mode), Op, Exp mode)]


