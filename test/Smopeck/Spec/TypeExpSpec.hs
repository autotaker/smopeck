{-# LANGUAGE DataKinds #-}
module Smopeck.Spec.TypeExpSpec where

import qualified Data.Map              as M
import           Smopeck.Spec.Exp
import           Smopeck.Spec.TypeExp
import           Smopeck.Spec.TypeUtil

import           Test.Hspec


spec :: Spec
spec = describe "Smopeck.Spec.TypeExp" $ do
    describe "evalTypeExp" $ do
        it "distribute type conditions" $ do
            let assert = Exp $ App Eq [Literal (LString "hoge"), Literal (LString "fuga")]
                input = fCond assert $ fInt []
                expected = fInt [] `withCond` assert
                env = M.empty
            evalTypeExp env input `shouldBe` expected
