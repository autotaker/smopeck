module Smopeck.Spec.LexerSpec where

import           Smopeck.Spec.Lexer
import           Test.Hspec

spec :: Spec
spec =
    describe "Smopeck.Spec.Parser" $ do
        it "lex type synonym def" $ do
            let tokens = [Type, TyName "Sample", Eq, TyName "String"]
            alexScanTokens "type Sample = String" `shouldBe` tokens
        it "lex empty endpoint def" $ do
            let tokens = [Endpoint, DQString "/", TyName "GET", Lbra, Rbra]
            alexScanTokens "endpoint \"/\" GET {}" `shouldBe` tokens


