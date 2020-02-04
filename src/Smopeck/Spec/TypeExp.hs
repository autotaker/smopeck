{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitForAll     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
module Smopeck.Spec.TypeExp where

import           Control.Monad
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
evalTypeExp env tyExp =
    toJoinNormalForm (tyExp >>= eval) >>= cata g
    where
    eval tyExp@TypeExpF{ typeExpName = Prim prim }
        = pure tyExp{ typeExpName = Prim prim }
    eval tyExp@TypeExpF{ typeExpName = User userType } =
        castFull $ fmap (extend bindName ext ref) typeDef
            where
            typeDef = env M.! userType
            ext = typeExpExt tyExp
            ref = typeExpRef tyExp
            bindName = typeExpBind tyExp
    g = CataMeet {
        fMTop = undefined,
        fMElem = pure,
        fMMeet = curry (join . uncurry (liftM2 intersect))
    }

extend :: VarName -> TypeExtension m -> TypeRefine m -> TypeExpF m WHNF -> TypeExpF m WHNF
extend bindName ext ref ty = ty {
        typeExpExt = extendTypeExt ext0 ext',
        typeExpRef = ref0 ++ ref'
    }
    where
        bindName0 = typeExpBind ty
        ext0 = typeExpExt ty
        ref0 = typeExpRef ty
        ext' = substTypeExt bindName bindName0 ext
        ref' = substTypeRefine bindName bindName0 ref

extendTypeExt :: TypeExtension m -> TypeExtension m -> TypeExtension m
extendTypeExt = M.unionWith (\a b -> b)

substTypeExt :: VarName -> VarName -> TypeExtension m -> TypeExtension m
substTypeExt bindNameFrom bindNameTo =
    fmap (substTypeExp bindNameFrom bindNameTo)

substTypeRefine :: VarName -> VarName -> TypeRefine m -> TypeRefine m
substTypeRefine bindNameFrom bindNameTo = map (\(lhs, op, rhs) ->
    (fmap (substExp bindNameFrom bindNameTo) lhs,
     op,
     substExp bindNameFrom bindNameTo rhs))

substExp :: VarName -> VarName -> Exp m -> Exp m
substExp bindNameFrom bindNameTo (Exp exp) =
    Exp (fmap (substLocation bindNameFrom bindNameTo) exp)

substLocation :: VarName -> VarName -> LocationF Root (Exp m) -> LocationF Root (Exp m)
substLocation bindNameFrom bindNameTo = fmap (substExp bindNameFrom bindNameTo)

substTypeExp :: VarName -> VarName -> TypeExp m HDefault -> TypeExp m HDefault
substTypeExp bindNameFrom bindNameTo = fmap $ \case
    ty | typeExpBind ty == bindNameFrom -> ty
       | typeExpBind ty == bindNameTo -> error "alpha rename"
       | otherwise -> ty{
            typeExpExt = substTypeExt bindNameFrom bindNameTo (typeExpExt ty),
            typeExpRef = substTypeRefine bindNameFrom bindNameTo (typeExpRef ty)
        }

intersect :: TypeExpF m WHNF -> TypeExpF m WHNF -> TypeExp m WHNF
intersect ty1 ty2
    | typeExpName ty1 /= typeExpName ty2 = LBot
    | typeExpName ty1 == typeExpName ty2 =
        pure ty1{
            typeExpExt = M.unionWith LMeet (typeExpExt ty1) ext2,
            typeExpRef = typeExpRef ty1 ++ ref2
        }
    where
        bindNameFrom = typeExpBind ty2
        bindNameTo = typeExpBind ty1
        ext2 = substTypeExt bindNameFrom bindNameTo (typeExpExt ty2)
        ref2 = substTypeRefine bindNameFrom bindNameTo (typeExpRef ty2)
