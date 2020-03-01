{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Smopeck.Spec.TypeExp where

import           Control.Monad
import           Data.Foldable
import           Data.Graph
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
        typeExpBind :: BindName mode,
        typeExpExt  :: TypeExtension mode,
        typeExpRef  :: TypeRefine mode
    }

deriving instance (Show (Exp mode), Show (TypeName head), Show (BindName mode), Show (TypeExtension mode) ) =>
    Show (TypeExpF mode head)
deriving instance (Eq (Exp mode), Eq (TypeName head), Eq (BindName mode), Eq (TypeExtension mode) ) =>
    Eq (TypeExpF mode head)


-- type TypeExp mode head = Lattice Full (TypeExpF mode head)
type family LatticeOf (head :: HeadMode) :: LatticeMode where
    LatticeOf HDefault = Full
    LatticeOf WHNF = Join

type TypeExp mode head = Lattice (LatticeOf head) (TypeExpF mode head)

data HeadMode = HDefault | WHNF

data TypeName (head :: HeadMode) where
    Prim :: Primitive -> TypeName head
    User :: String -> TypeName HDefault

data BindName (head :: Mode) where
    BindName :: String -> BindName Parsed
    BindDebrujin :: BindName Desugar

deriving instance Eq (TypeName h)
deriving instance Ord (TypeName h)
deriving instance Show (TypeName h)
deriving instance Eq (BindName m)
deriving instance Ord (BindName m)
deriving instance Show (BindName m)

instance Read (TypeName HDefault) where
    readPrec = do
        Ident x <- lexP
        case x of
            "Object" -> pure $ Prim PObject
            "String" -> pure $ Prim PString
            "Number" -> pure $ Prim PNumber
            "Int"    -> pure $ Prim PInt
            "Array"  -> pure $ Prim PArray
            "Bool"   -> pure $ Prim PBool
            "Null"   -> pure $ Prim PNull
            _        -> pure $ User x

type Route = String
type VarName = String
type UserType = String

type DefaultTypeEnv m = M.Map UserType (TypeExp m HDefault)
type WHNFTypeEnv m = M.Map UserType (TypeExp m WHNF)
data Primitive = PObject | PString | PNumber | PInt | PArray | PBool | PNull
    deriving(Eq,Ord,Show)
type TypeExtension mode = M.Map (Field (BindName mode)) (TypeExp mode HDefault)

newtype Exp mode = Exp (ExpF mode (LocationExp mode))

deriving instance Eq (Exp Desugar)
deriving instance Ord (Exp Desugar)
deriving instance Show (Exp Desugar)
deriving instance Eq (Exp Parsed)
deriving instance Ord (Exp Parsed)
deriving instance Show (Exp Parsed)
type LocationExp mode = LocationF (Root RootAny) (Exp mode)
type TypeRefine mode = [ (Op, Exp mode)]

evalTypeEnv :: DefaultTypeEnv Desugar -> WHNFTypeEnv Desugar
evalTypeEnv env = env'
    where
    env' = foldl' (\acc v ->
        let (def, tyName, _) = idx v
            def' = evalTypeExp acc def in
        M.insert tyName def' acc) M.empty (topSort graph)
    graph :: Graph
    (graph, idx) = graphFromEdges' $ do
        (tyName, def) <- M.toList env
        pure (def, tyName, dependency def)
    dependency :: TypeExp Desugar HDefault -> [UserType]
    dependency def = do
        tyExp <- toList def
        case typeExpName tyExp of
            Prim _ -> []
            User s -> pure s


evalTypeExp :: WHNFTypeEnv Desugar -> TypeExp Desugar HDefault -> TypeExp Desugar WHNF
evalTypeExp env tyExp =
    toJoinNormalForm (tyExp >>= eval) >>= cata g
    where
    eval tyExp@TypeExpF{ typeExpName = Prim prim }
        = pure tyExp{ typeExpName = Prim prim }
    eval tyExp@TypeExpF{ typeExpName = User userType } =
        castFull $ fmap (extend bindName ext ref) typeDef
            where
            typeDef = case M.lookup userType env of
                Just v  -> v
                Nothing -> error $ "undefined or cycle type: " ++ userType
            ext = typeExpExt tyExp
            ref = typeExpRef tyExp
            bindName = typeExpBind tyExp
    g = CataMeet {
        fMTop = undefined,
        fMElem = pure,
        fMMeet = curry (join . uncurry (liftM2 intersect))
    }

extend :: BindName Desugar -> TypeExtension Desugar
            -> TypeRefine Desugar -> TypeExpF Desugar WHNF
            -> TypeExpF Desugar WHNF
extend bindName ext ref ty = ty {
        typeExpExt = extendTypeExt ext0 ext',
        typeExpRef = ref0 ++ ref'
    }
    where
        bindName0 = typeExpBind ty
        ext0 = typeExpExt ty
        ref0 = typeExpRef ty
        ext' = ext
        ref' = ref

extendTypeExt :: TypeExtension m -> TypeExtension m -> TypeExtension m
extendTypeExt = M.unionWith (\a b -> b)

{-
substTypeExt :: VarName -> Int -> TypeExtension m -> TypeExtension m
substTypeExt bindNameFrom bindNameTo =
    fmap (substTypeExp bindNameFrom bindNameTo)

substTypeRefine :: VarName -> Int -> TypeRefine m -> TypeRefine m
substTypeRefine bindNameFrom bindNameTo = map (\(lhs, op, rhs) ->
    (fmap (substExp bindNameFrom bindNameTo) lhs,
     op,
     substExp bindNameFrom bindNameTo rhs))

substExp :: VarName -> Int -> Exp m -> Exp m
substExp bindNameFrom bindNameTo (Exp exp) =
    Exp (fmap (substLocation bindNameFrom bindNameTo) exp)

substLocation :: VarName -> Int -> LocationF Root (Exp m) -> LocationF Root (Exp m)
substLocation bindNameFrom bindNameTo = fmap (substExp bindNameFrom bindNameTo)

substTypeExp :: VarName -> Int -> TypeExp m HDefault -> TypeExp m HDefault
substTypeExp bindNameFrom bindNameTo = fmap $ \case
    ty | BindName s <- typeExpBind ty, s == bindNameFrom -> ty
       | otherwise -> ty{
            typeExpExt = substTypeExt bindNameFrom (bindNameTo + 1) (typeExpExt ty),
            typeExpRef = substTypeRefine bindNameFrom (bindNameTo + 1) (typeExpRef ty)
        }
        -}

intersect :: TypeExpF Desugar WHNF -> TypeExpF Desugar WHNF -> TypeExp Desugar WHNF
intersect ty1 ty2
    | typeExpName ty1 /= typeExpName ty2 = LBot
    | typeExpName ty1 == typeExpName ty2 =
        pure ty1{
            typeExpExt = M.unionWith LMeet (typeExpExt ty1) ext2,
            typeExpRef = typeExpRef ty1 ++ ref2
        }
    where
        ext2 = typeExpExt ty2
        ref2 = typeExpRef ty2
