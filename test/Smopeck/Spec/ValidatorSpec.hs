{-# LANGUAGE OverloadedStrings #-}
module Smopeck.Spec.ValidatorSpec where

import           Control.Monad.Except
import           Data.Aeson
import           Data.Either
import qualified Data.Map               as M
import           Smopeck.Spec.Syntax
import           Smopeck.Spec.Validator
import           Test.Hspec

spec :: Spec
spec =
    describe "Smopeck.Spec.Validator.validateJson" $ do
        it "String literal has String type" $ do
            let tyEnv = M.empty
                env = M.empty
                value = String "Hello"
                ty = TypeExp (Prim PString) [] []
            runExcept (validateJson tyEnv env value ty) `shouldBe` Right ()
        it "0 is not String type" $ do
            let tyEnv = M.empty
                env = M.empty
                value = String "hello"
                ty = TypeExp (Prim PNumber) [] []
            runExcept (validateJson tyEnv env value ty) `shouldSatisfy` isLeft

        it "0 has Number type" $ do
            let tyEnv = M.empty
                env = M.empty
                value = Number 0
                ty = TypeExp (Prim PNumber) [] []
            runExcept (validateJson tyEnv env value ty) `shouldBe` Right ()
        it "object has Object type" $ do
            let tyEnv = M.empty
                env = M.empty
                value = object []
                ty = TypeExp (Prim PObject) [] []
            runExcept (validateJson tyEnv env value ty) `shouldBe` Right ()
        it "null has Null type" $ do
            let tyEnv = M.empty
                env = M.empty
                value = Null
                ty = TypeExp (Prim PNull) [] []
            runExcept (validateJson tyEnv env value ty) `shouldBe` Right ()
        it "false has Bool type" $ do
            let tyEnv = M.empty
                env = M.empty
                value = Bool False
                ty = TypeExp (Prim PBool) [] []
            runExcept (validateJson tyEnv env value ty) `shouldBe` Right ()
        it "union type accepts both types" $ do
            let tyEnv = M.empty
                env = M.empty
                ty1 = TypeExp (Prim PBool) [] []
                ty2 = TypeExp (Prim PNull) [] []
                ty = UnionType ty1 ty2
            runExcept (validateJson tyEnv env (Bool False) ty) `shouldBe` Right ()
            runExcept (validateJson tyEnv env Null ty) `shouldBe` Right ()
