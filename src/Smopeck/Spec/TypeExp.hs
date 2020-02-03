{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitForAll     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
module Smopeck.Spec.TypeExp where

import           Data.Kind
import qualified Data.Map              as M
import           Data.Type.Bool
import           Data.Type.Equality
import           Smopeck.Mock.Location
import           Smopeck.Spec.Exp
import           Smopeck.Spec.Lattice
import           Text.Read             hiding (Number, String)

data TypeExpF (mode :: Mode) (head :: HeadMode) =
    TypeExpF {
        typeExpName :: TypeName head,
        typeExpBind :: VarName,
        typeExpExt  :: TypeExtension mode,
        typeExpRef  :: TypeRefine mode
    }

-- type TypeExp mode head = Lattice Full (TypeExpF mode head)
type family LatticeOf (head :: HeadMode) :: LatticeMode where
    LatticeOf HDefault = Full
    LatticeOf WHNF = Join

type TypeExp mode head = Lattice (LatticeOf head) (TypeExpF mode head)

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
type TypeExtension mode = M.Map FieldName (TypeExp mode HDefault)

newtype Exp mode = Exp (ExpF mode (LocationF Root (Exp mode)))

type TypeRefine mode = [ (RLocationF (Exp mode), Op, Exp mode)]

evalTypeExp :: WHNFTypeEnv m -> TypeExp m HDefault -> TypeExp m WHNF
evalTypeExp env (LElem TypeExpF{
    typeExpName = Prim prim,
    typeExpExt = ext,
    typeExpRef = ref,
    typeExpBind = bindName
    }) = LElem TypeExpF{
        typeExpName = Prim prim,
        typeExpExt = ext,
        typeExpRef = ref,
        typeExpBind = bindName
        }
evalTypeExp env (LElem TypeExpF{
    typeExpName = User userType,
    typeExpExt = ext,
    typeExpRef = ref,
    typeExpBind = bindName
    }) = extend bindName ext ref typeDef
        where
        typeDef = env M.! userType
evalTypeExp env (LJoin ty1 ty2) =
    LJoin (evalTypeExp env ty1) (evalTypeExp env ty2)
evalTypeExp env (LMeet ty1 ty2) =
    intersect (evalTypeExp env ty1) (evalTypeExp env ty2)

extend :: VarName -> TypeExtension m -> TypeRefine m -> TypeExp m WHNF -> TypeExp m WHNF
extend bindName ext ref = go
    where
    go (LJoin ty1 ty2)= LJoin (go ty1) (go ty2)
    go (LElem TypeExpF{
        typeExpName = typeName,
        typeExpBind = bindName0,
        typeExpExt = ext0,
        typeExpRef = ref0
    }) = LElem TypeExpF {
            typeExpName = typeName,
            typeExpBind = bindName0,
            typeExpExt = extendTypeExt ext0 ext',
            typeExpRef = ref0 ++ ref'
        }
        where
        ext' = substTypeExt bindName bindName0 ext
        ref' = substTypeRefine bindName bindName0 ref

extendTypeExt :: TypeExtension m -> TypeExtension m -> TypeExtension m
extendTypeExt = M.unionWith (\a b -> b)

substTypeExt :: VarName -> VarName -> TypeExtension m -> TypeExtension m
substTypeExt bindNameFrom bindNameTo = fmap (substTypeExp bindNameFrom bindNameTo)

substTypeRefine :: VarName -> VarName -> TypeRefine m -> TypeRefine m
substTypeRefine bindNameFrom bindNameTo = map (\(lhs, op, rhs) ->
    (fmap (substExp bindNameFrom bindNameTo) lhs, op, substExp bindNameFrom bindNameTo rhs))

substExp :: VarName -> VarName -> Exp m -> Exp m
substExp bindNameFrom bindNameTo (Exp exp) =
    Exp (fmap (substLocation bindNameFrom bindNameTo) exp)

substLocation :: VarName -> VarName -> LocationF Root (Exp m) -> LocationF Root (Exp m)
substLocation bindNameFrom bindNameTo = fmap (substExp bindNameFrom bindNameTo)

substTypeExp :: VarName -> VarName -> TypeExp m HDefault -> TypeExp m HDefault
substTypeExp bindNameFrom bindNameTo = go
    where
        go (LElem ty)
            | typeExpBind ty == bindNameFrom = LElem ty
            | typeExpBind ty == bindNameTo = error "alpha rename"
            | otherwise = LElem ty{
                typeExpExt = substTypeExt bindNameFrom bindNameTo (typeExpExt ty),
                typeExpRef = substTypeRefine bindNameFrom bindNameTo (typeExpRef ty)
            }
        go (LJoin ty1 ty2) = LJoin (go ty1) (go ty2)
        go (LMeet ty1 ty2) = LMeet (go ty1) (go ty2)

intersect :: TypeExp m WHNF -> TypeExp m WHNF -> TypeExp m WHNF
intersect ty1 (LJoin ty2a ty2b) =
    LJoin (intersect ty1 ty2a) (intersect ty1 ty2b)
intersect (LJoin ty1a ty1b) ty2 =
    LJoin (intersect ty1a ty2) (intersect ty1b ty2)
intersect (LElem ty1) (LElem ty2)
    | typeExpName ty1 /= typeExpName ty2 = LBot
    | typeExpName ty1 == typeExpName ty2 =
        LElem TypeExpF {
            typeExpName = typeExpName ty1,
            typeExpBind = typeExpBind ty1,
            typeExpExt = M.unionWith LMeet (typeExpExt ty1) ext2,
            typeExpRef = typeExpRef ty1 ++ ref2
        }
    where
        bindNameFrom = typeExpBind ty2
        bindNameTo = typeExpBind ty1
        ext2 = substTypeExt bindNameFrom bindNameTo (typeExpExt ty2)
        ref2 = substTypeRefine bindNameFrom bindNameTo (typeExpRef ty2)

