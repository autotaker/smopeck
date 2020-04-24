{-# LANGUAGE DataKinds #-}
module Smopeck.Spec.ParserSpec where

import           Smopeck.Spec.Lexer
import           Smopeck.Spec.Parser
import           Smopeck.Spec.Syntax  hiding (Eq, Gt, Lt, Match)
import qualified Smopeck.Spec.Syntax  as Syntax
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
            runLexerMock parse tokens `shouldBe` Right [TypeDef "Sample" (fTypeExp "Json" "." [(FieldString "name", fTypeExp "String" "." [] [])] [])]
        it "field can contain upper identifier" $ do
            let tokens = [Type, UpperId "Sample", Eq, UpperId "Json", Lbra, UpperId "Name" , Colon, UpperId "String", Rbra]
            runLexerMock parse tokens `shouldBe` Right [TypeDef "Sample" (fTypeExp "Json" "." [(FieldString "Name", fTypeExp "String" "." [] [])] [])]
        it "field can contain figures if it is quoted" $ do
            let tokens = [Type, UpperId "Sample", Eq, UpperId "Json", Lbra, SQString "Content-Type" , Colon, UpperId "String", Rbra]
            runLexerMock parse tokens `shouldBe` Right [TypeDef "Sample" (fTypeExp "Json" "." [(FieldString "Content-Type", fTypeExp "String" "." [] [])] [])]

        it "parse empty endpoint def" $ do
            let tokens = [Endpoint, DQString "/", UpperId "GET", Lbra, Rbra]
            runLexerMock parse tokens `shouldBe` Right [EndpointDef "/" "GET" []]
        it "parse regex match expression" $ do
            -- type Sample = String [ . =~ r'hoge' ]
            let tokens = [Type, UpperId "Sample", Eq, UpperId "String", Lsq, Dot, Match, SQRegex "[a-z]*", Rsq ]
                expected = TypeDef "Sample" (fTypeExp "String" "." [] [ (Syntax.Match, T.Exp $ Literal $ LRegex "[a-z]*" )])
            runLexerMock parse tokens `shouldBe` Right [expected]
