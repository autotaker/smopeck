{-# LANGUAGE DataKinds #-}
module Smopeck.Spec.DesugarSpec where

import qualified Data.Map              as M
import           Smopeck.Mock.Location
import           Smopeck.Spec.Desugar
import           Smopeck.Spec.Exp
import           Smopeck.Spec.Lattice
import qualified Smopeck.Spec.Syntax   as Syntax
import           Smopeck.Spec.TypeExp
import           Smopeck.Spec.TypeUtil as Desugar
import           Test.Hspec

spec :: Spec
spec = do
    describe "Smopeck.Spec.Desugar.desugarTypeExp" $ do
        it "test" $ do
            let tyInt = Syntax.fTypeExp "Int" "i" [] [ (Eq, Exp $ Var (Root (Absolute "obj")))]
                tyObj :: TypeExp 'Parsed 'HDefault
                tyObj = Syntax.fTypeExp "Object" "obj" [ (FieldString "hoge", (tyInt, Mandatory))] []
                tyInt' = Desugar.fInt [ (Eq, Exp $ Var (Root (Relative 1)))]
                tyObj' :: TypeExp 'Desugar 'HDefault
                tyObj' = Desugar.fObject (M.fromList [ (FieldString "hoge", (tyInt', Mandatory))])
            desugarTypeExp [] tyObj `shouldBe` tyObj'
        it "desugar literal type" $ do
            let tyLit = LElem $ LiteralType (LNumber 0)
                expected = Desugar.fNumber [ (Eq, Exp $ Literal (LNumber 0))]
            desugarTypeExp [] tyLit `shouldBe` expected
        it "desugar dq string literal type" $ do
            let tyLit = LElem $ LiteralType (LDQString "${obj.string}")
                expected = Desugar.fString [(Eq,Exp (App Add [Literal (LString ""),App (Func "str") [Var (Chain (Root (Relative 1)) (FieldString "string") Mandatory)]]))]
            desugarTypeExp ["obj"] tyLit `shouldBe` expected

    describe "Smopeck.Spec.Desugar.desugarString" $
        it "test" $ do
            let input = "hoge ${piyo} fuga"
                expected = App Add [Literal (LString "hoge "),App (Func "str") [Var (Root (Absolute "piyo"))],Literal (LString " fuga")]
            desugarString [] input `shouldBe` expected




