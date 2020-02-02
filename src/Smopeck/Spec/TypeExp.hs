{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
module Smopeck.Spec.TypeExp where

import qualified Data.Map              as M
import           Smopeck.Mock.Location
import           Smopeck.Spec.Exp
import           Text.Read             hiding (Number, String)

data TypeExpF (mode :: Mode) (head :: HeadMode) =
    TypeExpF {
        typeExpName :: TypeName head,
        typeExpBind :: VarName,
        typeExpExt  :: TypeExtension mode,
        typeExpRef  :: TypeRefine mode
    }

data TypeExp mode head where
    TypeExp :: TypeExpF mode head -> TypeExp mode head
    TypeUnion :: TypeExp mode head -> TypeExp mode head -> TypeExp mode head
    TypeIntersection :: TypeExp mode HDefault -> TypeExp mode HDefault -> TypeExp mode HDefault

{-
data TypeExpF (mode :: Mode) head =
    | T
    | UnionType (TypeExp mode head) (TypeExp mode head)
    | IntersectionType (TypeExp mode head) (TypeExp mode head)
    deriving(Functor)

-}

data HeadMode = HDefault | WHNF

data TypeName (head :: HeadMode) where
    Prim :: Primitive -> TypeName head
    User :: String -> TypeName HDefault

deriving instance Eq (TypeName HDefault)
deriving instance Eq (TypeName WHNF)
deriving instance Ord (TypeName HDefault)
deriving instance Ord (TypeName WHNF)
deriving instance Show (TypeName HDefault)
deriving instance Show (TypeName WHNF)

instance Read (TypeName HDefault) where
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
type UserType = String

type DefaultTypeEnv m = M.Map UserType (TypeExp m HDefault)
type WHNFTypeEnv m = M.Map UserType (TypeExp m WHNF)
data Primitive = PObject | PString | PNumber | PArray | PBool | PNull
    deriving(Eq,Ord,Show)
type TypeExtension mode = [ (FieldName, TypeExp mode HDefault)]

newtype Exp mode = Exp (ExpF mode (LocationF Root (Exp mode)))

type TypeRefine mode = [ (RLocationF (Exp mode), Op, Exp mode)]

evalTypeExp :: WHNFTypeEnv m -> TypeExp m HDefault -> TypeExp m WHNF
evalTypeExp env (TypeExp TypeExpF{
    typeExpName = Prim prim,
    typeExpExt = [],
    typeExpRef = ref,
    typeExpBind = bindName
    }) = TypeExp TypeExpF{
        typeExpName = Prim prim,
        typeExpExt = [],
        typeExpRef = ref,
        typeExpBind = bindName
        }
evalTypeExp env (TypeExp TypeExpF{
    typeExpName = User userType,
    typeExpExt = ext,
    typeExpRef = ref,
    typeExpBind = bindName
    }) = extend typeDef ext bindName ref
        where
        typeDef = env M.! userType
evalTypeExp env (TypeUnion ty1 ty2) =
    TypeUnion (evalTypeExp env ty1) (evalTypeExp env ty2)
evalTypeExp env (TypeIntersection ty1 ty2) =
    intersect (evalTypeExp env ty1) (evalTypeExp env ty2)

extend :: TypeExp m WHNF -> TypeExtension m -> VarName -> TypeRefine m -> TypeExp m WHNF
extend _ _ _ _ = undefined

intersect :: TypeExp m WHNF -> TypeExp m WHNF -> TypeExp m WHNF
intersect _ _ = undefined
