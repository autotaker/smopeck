module Smopeck.Spec.ParserSpec where

import           Smopeck.Spec.Lexer
import           Smopeck.Spec.Parser
import           Smopeck.Spec.Syntax
import           Test.Hspec

spec :: Spec
spec =
    describe "Smopeck.Spec.Parser" $ do
        it "parse type synonym def" $ do
            let tokens = [Type, TyName "Sample", Eq, TyName "String"]
            parseDef tokens `shouldBe` TypeDef "Sample" (TypeExp "String" [] [])

        it "parse empty endpoint def" $ do
            let tokens = [Endpoint, DQString "/", TyName "GET", Lbra, Rbra]
            parseDef tokens `shouldBe` EndpointDef "/" "GET" []

