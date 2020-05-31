{-# LANGUAGE OverloadedStrings #-}
module Smopeck.Spec.ValidatorSpec where

import           Control.Monad.Except
import           Data.Aeson
import           Data.Either
import qualified Data.Map               as M
import           Smopeck.Mock.Location
import           Smopeck.Spec.Exp
import           Smopeck.Spec.Lattice
import           Smopeck.Spec.TypeExp
import           Smopeck.Spec.TypeUtil
import           Smopeck.Spec.Validator
import           Test.Hspec

spec :: Spec
spec = do
    describe "Smopeck.Spec.Validator.validateJson" $ do
        it "String literal has String type" $ do
            let tyEnv = M.empty
                env = M.singleton "it" value
                value = String "Hello"
                ty = fString []
            validateJson tyEnv env "it" ty `shouldBe` Right ()
        it "0 is not String type" $ do
            let tyEnv = M.empty
                env = M.singleton "it" value
                value = String "hello"
                ty = fNumber []
            validateJson tyEnv env "it" ty `shouldSatisfy` isLeft

        it "0 has Number type" $ do
            let tyEnv = M.empty
                env = M.singleton "it" value
                value = Number 0
                ty = fNumber []
            validateJson tyEnv env "it" ty `shouldBe` Right ()
        it "object has Object type" $ do
            let tyEnv = M.empty
                env = M.singleton "it" value
                value = object []
                ty = fObject M.empty
            validateJson tyEnv env "it" ty `shouldBe` Right ()
        it "null has Null type" $ do
            let tyEnv = M.empty
                env = M.singleton "it" value
                value = Null
                ty = fNull
            validateJson tyEnv env "it" ty `shouldBe` Right ()
        it "false has Bool type" $ do
            let tyEnv = M.empty
                env = M.singleton "it" value
                value = Bool False
                ty = fBool []
            validateJson tyEnv env "it" ty `shouldBe` Right ()
        it "union type accepts both types" $ do
            let tyEnv = M.empty
                env = M.fromList [ ("a", Bool False) , ("b", Null)]
                ty1 = fBool []
                ty2 = fNull
                ty = LJoin ty1 ty2
            validateJson tyEnv env "a" ty `shouldBe` Right ()
            validateJson tyEnv env "b" ty `shouldBe` Right ()
        it "reject value if type condition does not hold" $ do
            let tyEnv = M.empty
                env = M.fromList [ ("a", Bool False) ]
                cond = Exp $ Var (Root (Absolute "a"))
                ty = fBool [] `withCond` cond
            validateJson tyEnv env "a" ty `shouldBe` Left "type cond does not hold"
        it "accept value if type condition holds" $ do
            let tyEnv = M.empty
                env = M.fromList [ ("a", Bool True) ]
                cond = Exp $ Var (Root (Absolute "a"))
                ty = fBool [] `withCond` cond
            validateJson tyEnv env "a" ty `shouldBe` Right ()

    describe "Smopec.Spec.Validator.parseParam" $ do
        it "can parse hoge as String" $
            runExcept (parseParam "hoge" (fString [])) `shouldBe` Right (String "hoge")
        it "can parse 1 as Int" $
            runExcept (parseParam "1" (fInt [])) `shouldBe` Right (Number 1)
        it "cannot parse float as Int" $
            runExcept (parseParam "3.14" (fInt [])) `shouldSatisfy` isLeft
        it "can parse float as Number" $
            runExcept (parseParam "3.14" (fNumber [])) `shouldBe` Right (Number 3.14)
        it "can parse true as Bool" $
            runExcept (parseParam "truE" (fBool [])) `shouldBe` Right (Bool True)
        it "can parse false as Bool" $
            runExcept (parseParam "False" (fBool [])) `shouldBe` Right (Bool False)
        it "can parse null as Null" $
            runExcept (parseParam "nulL" fNull) `shouldBe` Right Null


