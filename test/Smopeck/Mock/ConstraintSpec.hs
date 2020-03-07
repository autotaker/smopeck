{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Smopeck.Mock.ConstraintSpec where


import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.Map                as M
import           Smopeck.Mock.Constraint hiding (TypeExp)
import           Smopeck.Mock.Location
import           Smopeck.Spec.Exp
import           Smopeck.Spec.Lattice
import           Smopeck.Spec.TypeExp
import           Test.Hspec

fPrim :: Primitive -> TypeExtension 'Desugar -> TypeRefine 'Desugar -> TypeExp 'Desugar head
fPrim prim ext ref = LElem (TypeExpF (Prim prim) BindDebrujin  ext ref)

fInt, fBool, fNumber, fString :: TypeRefine 'Desugar -> TypeExp 'Desugar head
fInt = fPrim PInt M.empty
fBool = fPrim PBool M.empty
fNumber = fPrim PNumber M.empty
fString = fPrim PString M.empty
fObject :: TypeExtension 'Desugar -> TypeExp 'Desugar head
fObject ext = fPrim PObject ext []
fArray :: TypeExp 'Desugar 'HDefault -> TypeExp 'Desugar 'HDefault -> TypeExp 'Desugar head
fArray size elt = fPrim PArray (M.fromList [
    (FieldString "length", size),
    (FieldIndex BindDebrujin , elt)]) []

spec :: Spec
spec =
    describe "Smopeck.Mock.Constraint.evalL" $ do
        it "gen number" $ do
            let ty = fNumber []
            v <- mockJson M.empty ty
            print v
        it "gen number in range" $ do
            let ty = fNumber [(Eq, Exp $ Literal (LNumber 0))]
            v <- mockJson M.empty ty
            v `shouldBe` toJSON (0 :: Int)
            print v
        it "gen object" $ do
            let tyInt = fInt []
                tySize = fInt [(Gt, Exp $ Literal (LNumber 0)), (Lte, Exp $ Literal (LNumber 10))]
                tyStr = fString [(Eq, Exp $ Literal $ LString "fizz")]
                    `LJoin` fString [(Eq, Exp $ Literal $ LString "buzz")]
                ty =fObject (M.fromList [(FieldString "hoge", tyInt), (FieldString "fuga", tyStr)])
                ty2 = fArray tySize ty
            v <- mockJson M.empty ty2
            print $ v
        it "gen with env" $ do
            Just v <- decodeFileStrict "resource/test/context.json"
            let expected = [Number 1, Bool True, String "string", Null, toJSON [1,2,3::Int]]
                fields = [(PInt, "int"), (PBool, "bool"), (PString, "string"),(PNull, "null")]
            forM_ (zip fields expected) $ \((prim, field), expectedValue) -> do
                let ty = fPrim prim M.empty [(Eq, Exp $ Var (Root (Absolute "ctx") `Field` field))]
                mockValue <- mockJsonWithEnv M.empty (M.fromList [("ctx", v)]) ty
                mockValue `shouldBe` expectedValue

