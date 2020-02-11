module Smopeck.Spec.ParserSpec where

import qualified Data.Map            as M
import           Smopeck.Spec.Lexer
import           Smopeck.Spec.Parser
import           Smopeck.Spec.Syntax
import           Test.Hspec

spec :: Spec
spec = pure ()
{-
    describe "Smopeck.Spec.Parser" $ do
        it "parse type synonym def" $ do
            let tokens = [Type, UpperId "Sample", Eq, UpperId "String"]
            runLexerMock parse tokens `shouldBe` Right (TypeDef (User "Sample") (TypeExp (Prim PString) M.empty []))
        it "parse type synonym def" $ do
            let tokens = [Type, UpperId "Sample", Eq, UpperId "Json", Lbra, LowerId "name" , Colon, UperId "String", Rbra]
            runLexerMock parse tokens `shouldBe` Right (TypeDef (User "Sample") (TypeExp (User "Json") "." (M.fromList [("name", TypeExp (Prim PString) "." M.empty [])]) []))

        it "parse empty endpoint def" $ do
            let tokens = [Endpoint, DQString "/", UpperId "GET", Lbra, Rbra]
            runLexerMock parse tokens `shouldBe` Right (EndpointDef "/" "GET" [])

-}
