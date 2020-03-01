{-# LANGUAGE DataKinds #-}
module Smopeck.Spec.ParserSpec where

import           Smopeck.Spec.Lexer
import           Smopeck.Spec.Parser
import           Smopeck.Spec.Syntax  hiding (Eq, Gt, Lt)
import qualified Smopeck.Spec.TypeExp as T
import           Test.Hspec

spec :: Spec
spec =
    describe "Smopeck.Spec.Parser" $ do
        it "read typename" $
            read "String" `shouldBe` (T.Prim T.PString :: T.TypeName 'T.HDefault)
        it "parse type synonym def" $ do
            let tokens = [Type, UpperId "Sample", Eq, UpperId "String"]
            runLexerMock parse tokens `shouldBe` Right [TypeDef "Sample" (fTypeExp "String" "." [] [])]
        it "parse type synonym def" $ do
            let tokens = [Type, UpperId "Sample", Eq, UpperId "Json", Lbra, LowerId "name" , Colon, UpperId "String", Rbra]
            runLexerMock parse tokens `shouldBe` Right [TypeDef "Sample" (fTypeExp "Json" "." ([("name", fTypeExp "String" "." [] [])]) [])]

        it "parse empty endpoint def" $ do
            let tokens = [Endpoint, DQString "/", UpperId "GET", Lbra, Rbra]
            runLexerMock parse tokens `shouldBe` Right [EndpointDef "/" "GET" []]
