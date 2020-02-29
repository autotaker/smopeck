{-# LANGUAGE DataKinds #-}
module Smopeck.Spec.DesugarSpec where

import qualified Data.Map              as M
import           Smopeck.Mock.Location
import           Smopeck.Spec.Desugar
import           Smopeck.Spec.Exp
import           Smopeck.Spec.Lattice
import           Smopeck.Spec.TypeExp
import           Test.Hspec

spec :: Spec
spec = describe "Smopeck.Spec.Desugar.desugarTypeExp" $ do
    it "test" $ do
        let tyInt = LElem $ TypeExpF (Prim PInt) (BindName "i") M.empty [ (Eq, Exp $ Var (Root (Absolute "obj")))]
            tyObj :: TypeExp 'Parsed 'HDefault
            tyObj = LElem $ TypeExpF (Prim PObject) (BindName "obj") (M.fromList [ ("hoge", tyInt)]) []
            tyInt' = LElem $ TypeExpF (Prim PInt) BindDebrujin M.empty [ (Eq, Exp $ Var (Root (Relative 1)))]
            tyObj' :: TypeExp 'Desugar 'HDefault
            tyObj' = LElem $ TypeExpF (Prim PObject) BindDebrujin (M.fromList [ ("hoge", tyInt')]) []
        desugarTypeExp [] tyObj `shouldBe` tyObj'





