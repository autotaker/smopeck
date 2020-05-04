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
spec = do
    describe "Smopeck.Spec.Desugar.desugarTypeExp" $
        it "test" $ do
            let tyInt = LElem $ TypeExpF (Prim PInt) (BindName "i") M.empty [ (Eq, Exp $ Var (Root (Absolute "obj")))]
                tyObj :: TypeExp 'Parsed 'HDefault
                tyObj = LElem $ TypeExpF (Prim PObject) (BindName "obj") (M.fromList [ (FieldString "hoge", tyInt)]) []
                tyInt' = LElem $ TypeExpF (Prim PInt) BindDebrujin M.empty [ (Eq, Exp $ Var (Root (Relative 1)))]
                tyObj' :: TypeExp 'Desugar 'HDefault
                tyObj' = LElem $ TypeExpF (Prim PObject) BindDebrujin (M.fromList [ (FieldString "hoge", tyInt')]) []
            desugarTypeExp [] tyObj `shouldBe` tyObj'
    describe "Smopeck.Spec.Desugar.desugarString" $
        it "test" $ do
            let input = "hoge ${piyo} fuga"
                expected = App Add [Literal (LString "hoge "),App (Func "str") [Var (Root (Absolute "piyo"))],Literal (LString " fuga")]
            desugarString [] input `shouldBe` expected





