{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Smopeck.Mock.ConstraintSpec where


import           Control.Exception.Base
import           Control.Monad.Reader
import           Data.Aeson
import           Data.List               (isPrefixOf)
import qualified Data.Map                as M
import           Smopeck.Mock.Constraint hiding (TypeExp)
import           Smopeck.Mock.Location
import           Smopeck.Spec.Exp
import           Smopeck.Spec.Lattice
import           Smopeck.Spec.TypeExp
import           Smopeck.Spec.TypeUtil
import           Test.Hspec

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
        it "gen object" $ do
            let tyInt = fInt []
                tySize = fInt [(Gt, Exp $ Literal (LNumber 0)), (Lte, Exp $ Literal (LNumber 10))]
                tyStr = fString [(Eq, Exp $ Literal $ LString "fizz")]
                    `LJoin` fString [(Eq, Exp $ Literal $ LString "buzz")]
                ty =fObject (M.fromList [(FieldString "hoge", (tyInt,Mandatory)), (FieldString "fuga", (tyStr,Mandatory))])
                ty2 = fArray tySize ty
            v <- mockJson M.empty ty2
            print $ v
        it "gen with env" $ do
            Just v <- decodeFileStrict "resource/test/context.json"
            let expected = [Number 1, Bool True, String "string", Null, toJSON [1,2,3::Int]]
                fields = [(PInt, "int"), (PBool, "bool"), (PString, "string"),(PNull, "null")]
            forM_ (zip fields expected) $ \((prim, field), expectedValue) -> do
                let ty = fPrim prim M.empty [(Eq, Exp $ Var (Field (Root (Absolute "ctx")) field Mandatory))]
                mockValue <- mockJsonWithEnv M.empty (M.fromList [("ctx", v)]) ty
                mockValue `shouldBe` expectedValue
        it "not gen unsatisfiable cond" $ do
            let input = fInt [] `withCond` (Exp $ Literal $ LBool False)
            mockJson M.empty input `shouldThrow` \(ErrorCallWithLocation err _) ->
                "no candidates:" `isPrefixOf` err
        it "gen satisfiable cond" $ do
            let input = fInt [] `withCond` (Exp $ Literal $ LBool True)
            v <- mockJson M.empty input
            v `shouldSatisfy` (\case
                Number _ -> True
                _        -> False)
        it "gen satisfiable cond" $ do
            let tyInt = fCond (Exp $ App Eq [Var piyo, Var piyo]) (fInt [])
                piyo = Root (Relative 1)
                -- get(i) : Object { hoge : Int ? i = i}
                fields = [ (FieldString "hoge", (tyInt,Mandatory))]
                tyArr = fArray (fInt []) tyObj
                tyObj = fObject (M.fromList fields)
            v <- mockJson M.empty tyArr
            v `shouldSatisfy` (\case
                Array _ -> True
                _       -> False)
