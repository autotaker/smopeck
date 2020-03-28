{-# LANGUAGE OverloadedStrings #-}
module Smopeck.Spec.ValidatorSpec where

import           Control.Monad.Except
import           Data.Aeson
import           Data.Either
import qualified Data.Map               as M
import           Smopeck.Spec.Lattice
import           Smopeck.Spec.TypeUtil
import           Smopeck.Spec.Validator
import           Test.Hspec

spec :: Spec
spec =
    describe "Smopeck.Spec.Validator.validateJson" $ do
        it "String literal has String type" $ do
            let tyEnv = M.empty
                env = M.singleton "it" value
                value = String "Hello"
                ty = fString []
            runExcept (validateJson tyEnv env "it" ty) `shouldBe` Right ()
        it "0 is not String type" $ do
            let tyEnv = M.empty
                env = M.singleton "it" value
                value = String "hello"
                ty = fNumber []
            runExcept (validateJson tyEnv env "it" ty) `shouldSatisfy` isLeft

        it "0 has Number type" $ do
            let tyEnv = M.empty
                env = M.singleton "it" value
                value = Number 0
                ty = fNumber []
            runExcept (validateJson tyEnv env "it" ty) `shouldBe` Right ()
        it "object has Object type" $ do
            let tyEnv = M.empty
                env = M.singleton "it" value
                value = object []
                ty = fObject M.empty
            runExcept (validateJson tyEnv env "it" ty) `shouldBe` Right ()
        it "null has Null type" $ do
            let tyEnv = M.empty
                env = M.singleton "it" value
                value = Null
                ty = fNull
            runExcept (validateJson tyEnv env "it" ty) `shouldBe` Right ()
        it "false has Bool type" $ do
            let tyEnv = M.empty
                env = M.singleton "it" value
                value = Bool False
                ty = fBool []
            runExcept (validateJson tyEnv env "it" ty) `shouldBe` Right ()
        it "union type accepts both types" $ do
            let tyEnv = M.empty
                env = M.fromList [ ("a", Bool False) , ("b", Null)]
                ty1 = fBool []
                ty2 = fNull
                ty = LJoin ty1 ty2
            runExcept (validateJson tyEnv env "a" ty) `shouldBe` Right ()
            runExcept (validateJson tyEnv env "b" ty) `shouldBe` Right ()
