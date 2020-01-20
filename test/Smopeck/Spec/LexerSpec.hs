module Smopeck.Spec.LexerSpec where

import           Smopeck.Spec.Lexer
import           Test.Hspec

spec :: Spec
spec =
    describe "Smopeck.Spec.Parser" $ do
        it "lex type synonym def" $ do
            let tokens = [Type, TyName "Sample", Eq, TyName "String"]
            alexScanTokens "type Sample = String" `shouldBe` tokens
        it "lex `type` as a reserved token only if it appears at the beginning of a line" $ do
            alexScanTokens "type" `shouldBe` [Type]
            alexScanTokens " type" `shouldBe` [Var "type"]
        it "lex `endpoint` as a reserved token only if it appears at the beginning of a line" $ do
            alexScanTokens "endpoint" `shouldBe` [Endpoint]
            alexScanTokens " endpoint" `shouldBe` [Var "endpoint"]
        it "lex empty endpoint def" $ do
            let tokens = [Endpoint, DQString "/", TyName "GET", Lbra, Rbra]
            alexScanTokens "endpoint \"/\" GET {}" `shouldBe` tokens


