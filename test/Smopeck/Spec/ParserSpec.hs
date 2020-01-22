module Smopeck.Spec.ParserSpec where

import           Smopeck.Spec.Lexer
import           Smopeck.Spec.Parser
import           Smopeck.Spec.Syntax
import           Test.Hspec

spec :: Spec
spec =
    describe "Smopeck.Spec.Parser" $ do
        it "parse type synonym def" $ do
            let tokens = [Type, UpperId "Sample", Eq, UpperId "String"]
            runLexerMock parse tokens `shouldBe` Right (TypeDef "Sample" (TypeExp "String" [] []))
        it "parse type synonym def" $ do
            let tokens = [Type, UpperId "Sample", Eq, UpperId "Json", Lbra, LowerId "name" , Colon, UpperId "String", Rbra]
            runLexerMock parse tokens `shouldBe` Right (TypeDef "Sample" (TypeExp "Json" [("name", TypeExp "String" [] [])] []))

        it "parse empty endpoint def" $ do
            let tokens = [Endpoint, DQString "/", UpperId "GET", Lbra, Rbra]
            runLexerMock parse tokens `shouldBe` Right (EndpointDef "/" "GET" [])

