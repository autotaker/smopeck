{-# LANGUAGE DataKinds #-}
module Smopeck.Spec.ExpSpec where

import qualified Data.Map         as M
import           Smopeck.Spec.Exp
import           Test.Hspec

type Exp = ExpF 'Desugar String

spec :: Spec
spec =
    describe "Smopeck.Spec.Exp.eval" $ do
        it "1 + 2 == 3" $ do
            let env = M.fromList [ ("a", LNumber 1), ("b", LNumber 2) ]
                expr = App Add [Var "a", Var "b"]
            eval env expr `shouldBe` Right (LNumber 3)
        it "1 - 2 == 3" $ do
            let env = M.fromList [ ("a", LNumber 1), ("b", LNumber 2) ]
                expr = App Sub [Var "a", Var "b"]
            eval env expr `shouldBe` Right (LNumber (-1))
        it "1 * 2 == 2" $ do
            let env = M.fromList [ ("a", LNumber 1), ("b", LNumber 2) ]
                expr = App Mul [Var "a", Var "b"]
            eval env expr `shouldBe` Right (LNumber 2)
        it "1 / 2 == 0.5" $ do
            let env = M.fromList [ ("a", LNumber 1), ("b", LNumber 2) ]
                expr = App Div [Var "a", Var "b"]
            eval env expr `shouldBe` Right (LNumber (1/2))
        it "1 = 2 == false" $ do
            let env = M.fromList [ ("a", LNumber 1), ("b", LNumber 2) ]
                expr = App Eq [Var "a", Var "b"]
            eval env expr `shouldBe` Right (LBool False)
        it "1 < 2 == true" $ do
            let env = M.fromList [ ("a", LNumber 1), ("b", LNumber 2) ]
                expr = App Lt [Var "a", Var "b"]
            eval env expr `shouldBe` Right (LBool True)
        it "1 > 2 == false" $ do
            let env = M.fromList [ ("a", LNumber 1), ("b", LNumber 2) ]
                expr = App Gt [Var "a", Var "b"]
            eval env expr `shouldBe` Right (LBool False)
        it "1 <= 2 == true" $ do
            let env = M.fromList [ ("a", LNumber 1), ("b", LNumber 2) ]
                expr = App Lte [Var "a", Var "b"]
            eval env expr `shouldBe` Right (LBool True)
        it "1 >= 2 == False" $ do
            let env = M.fromList [ ("a", LNumber 1), ("b", LNumber 2) ]
                expr = App Gte [Var "a", Var "b"]
            eval env expr `shouldBe` Right (LBool False)
        it "\"a\" + \"b\" == \"ab\"" $ do
            let expr :: Exp
                expr = App Add [Literal (LString "a"), Literal (LString "b")]
                env = M.fromList []
            eval env expr `shouldBe` Right (LString "ab")
        it "x == x" $ do
            let env = M.fromList [ ]
                expr = Var "x"
            eval env expr `shouldBe` Left "x"
        it "abracadabara =~ [abcdr]+ == True" $ do
            let lhs = LString "abracadabra"
                rhs = LRegex "[abcdr]+"
            interpret Match [lhs, rhs] `shouldBe` LBool True
        it "abracadabara =~ [abc]+ == False" $ do
            let lhs = LString "abracadabra"
                rhs = LRegex "[abc]+"
            interpret Match [lhs, rhs] `shouldBe` LBool False
        it "'hoge[]' + r'fuga' == r'hoge\\[\\]fuga' " $ do
            let lhs = LString "hoge[]"
                rhs = LRegex "fuga"
                expected = LRegex "hoge\\[]fuga"
            interpret Add [lhs, rhs] `shouldBe` expected
        it "str(0) == '0'" $ do
            let input = [LNumber 0]
                expected = LString "0.0"
            interpret (Func "str") input `shouldBe` expected
