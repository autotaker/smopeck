{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Smopeck.Spec.TypeExp where

import           Control.Monad
import           Control.Monad.Free
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

data TypeExpF (mode :: Mode) (head :: HeadMode) where
    TypeExpF :: {
        typeExpName :: TypeName head,
        typeExpBind :: BindName mode,
        typeExpExt  :: TypeExtension mode,
        typeExpRef  :: TypeRefine mode,
        -- Scope of condition expression does not include typeExpName
        typeExpCond :: TypeCond mode head
      } -> TypeExpF mode head
    LiteralType :: Literal Parsed -> TypeExpF Parsed head


deriving instance
    (Show (Exp mode),
     Show (TypeName head),
     Show (BindName mode),
     Show (TypeExtension mode),
     Show (TypeCond mode head)) => Show (TypeExpF mode head)
deriving instance
    (Eq (Exp mode),
     Eq (TypeName head),
     Eq (BindName mode),
     Eq (TypeExtension mode),
     Eq (TypeCond mode head)) => Eq (TypeExpF mode head)


-- type TypeExp mode head = Lattice Full (TypeExpF mode head)
type family LatticeOf (head :: HeadMode) :: LatticeMode where
    LatticeOf HDefault = Full
    LatticeOf WHNF = Join

type TypeExp mode head = LatticeExt (LatticeOf head) (LatticeExtOf mode head) (TypeExpF mode head)

type family LatticeExtOf mode head where
    LatticeExtOf mode HDefault = TypeCondF mode
    LatticeExtOf mode WHNF = VoidF

data TypeCondF mode a = HasCondF (Exp mode) a
    deriving(Functor, Foldable,Traversable)

deriving instance (Eq (Exp mode), Eq a) => Eq (TypeCondF mode a)
deriving instance (Show (Exp mode), Show a) => Show (TypeCondF mode a)

data HeadMode = HDefault | WHNF

data TypeName (head :: HeadMode) where
    Prim :: Primitive -> TypeName head
    User :: String -> TypeName HDefault

data BindName (head :: Mode) where
    BindName :: String -> BindName Parsed
    BindDebrujin :: BindName Desugar

data TypeCond (mode :: Mode) (head :: HeadMode) where
    NoCond :: TypeCond mode head
    HasCond :: Exp mode -> TypeCond mode WHNF

instance Semigroup (TypeCond mode head) where
    NoCond <> x = x
    x <> NoCond = x
    HasCond (Exp x) <> HasCond (Exp y) =
        HasCond (Exp (App And [x, y]))
instance Monoid (TypeCond mode head) where
    mempty = NoCond

deriving instance Eq (TypeName h)
deriving instance Ord (TypeName h)
deriving instance Show (TypeName h)
deriving instance Eq (BindName m)
deriving instance Ord (BindName m)
deriving instance Show (BindName m)
deriving instance Show (Exp m) => Show (TypeCond m h)
deriving instance Eq (Exp m) => Eq (TypeCond m h)

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

deriving instance (Show a, Show (Exp mode)) => Show (LatticeExt l (TypeCondF mode) a)
deriving instance (Eq a, Eq (Exp mode)) => Eq (LatticeExt l (TypeCondF mode) a)

evalTypeEnv :: DefaultTypeEnv Desugar -> WHNFTypeEnv Desugar
evalTypeEnv env = env'
    where
    env' = foldr (\v acc ->
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
    toJoinExtNormalForm (tyExp >>= eval) >>= cata g
    where
    eval :: TypeExpF Desugar HDefault -> LatticeExt Full (TypeCondF Desugar) (TypeExpF Desugar WHNF)
    eval tyExp@TypeExpF{ typeExpName = Prim prim }
        = pure tyExp{ typeExpName = Prim prim, typeExpCond = NoCond }
    eval tyExp@TypeExpF{ typeExpName = User userType } =
        -- castFull :: Ext
        castFull $ fmap (extend bindName ext ref) typeDef
            where
            typeDef = case M.lookup userType env of
                Just v  -> v
                Nothing -> error $ "undefined or cycle type: " ++ userType ++ " env: " ++ show env
            ext = typeExpExt tyExp
            ref = typeExpRef tyExp
            bindName = typeExpBind tyExp
    g = CataMeet {
        fMTop = undefined,
        fMElem = pure . evalCond,
        fMMeet = curry (join . uncurry (liftM2 intersect))
    }
    evalCond :: Free (TypeCondF Desugar) (TypeExpF Desugar WHNF) -> TypeExpF Desugar WHNF
    evalCond = iter $ \(HasCondF (Exp e) ty) ->
        ty { typeExpCond = HasCond (Exp (fmap pushVar e)) <> typeExpCond ty }
        where pushVar :: LocationExp Desugar -> LocationExp Desugar
              pushVar = mapRoot (\case
                Relative i -> Relative (i+1)
                Absolute s -> Absolute s)


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
