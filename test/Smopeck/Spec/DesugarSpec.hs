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
    describe "Smopeck.Spec.Desugar.desugarTypeExp" $ do
        it "test" $ do
            let tyInt = LElem $ TypeExpF (Prim PInt) (BindName "i") M.empty [ (Eq, Exp $ Var (Root (Absolute "obj")))]
                tyObj :: TypeExp 'Parsed 'HDefault
                tyObj = LElem $ TypeExpF (Prim PObject) (BindName "obj") (M.fromList [ (FieldString "hoge", tyInt)]) []
                tyInt' = LElem $ TypeExpF (Prim PInt) BindDebrujin M.empty [ (Eq, Exp $ Var (Root (Relative 1)))]
                tyObj' :: TypeExp 'Desugar 'HDefault
                tyObj' = LElem $ TypeExpF (Prim PObject) BindDebrujin (M.fromList [ (FieldString "hoge", tyInt')]) []
            desugarTypeExp [] tyObj `shouldBe` tyObj'
        it "desugar literal type" $ do
            let tyLit = LElem $ LiteralType (LNumber 0)
                expected = LElem $ TypeExpF (Prim PNumber) BindDebrujin M.empty [ (Eq, Exp $ Literal (LNumber 0))]
            desugarTypeExp [] tyLit `shouldBe` expected
        it "desugar dq string literal type" $ do
            let tyLit = LElem $ LiteralType (LDQString "${obj.string}")
                expected = LElem $ TypeExpF (Prim PString) BindDebrujin M.empty [(Eq,Exp (App Add [Literal (LString ""),App (Func "str") [Var (Chain (Root (Relative 1)) (FieldString "string"))]]))]
            desugarTypeExp ["obj"] tyLit `shouldBe` expected

    describe "Smopeck.Spec.Desugar.desugarString" $
        it "test" $ do
            let input = "hoge ${piyo} fuga"
                expected = App Add [Literal (LString "hoge "),App (Func "str") [Var (Root (Absolute "piyo"))],Literal (LString " fuga")]
            desugarString [] input `shouldBe` expected





