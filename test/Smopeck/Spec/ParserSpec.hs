{-# LANGUAGE DataKinds #-}
module Smopeck.Spec.ParserSpec where

import           Smopeck.Spec.Lexer
import           Smopeck.Spec.Parser
import qualified Smopeck.Spec.Syntax  as Syntax
import           Smopeck.Spec.Syntax  hiding (And, Eq, Gt, Lt, Match, Or)
import qualified Smopeck.Spec.TypeExp as T
import           Test.Hspec

spec :: Spec
spec = do
    describe "Smopeck.Spec.Parser.parse" $ do
        it "read typename" $
            read "String" `shouldBe` (T.Prim T.PString :: T.TypeName 'T.HDefault)
        it "parse type synonym def" $ do
            -- type Sample = String
            let tokens = [Type, UpperId "Sample", Eq, UpperId "String"]
            runLexerMock parse tokens `shouldBe` Right [TypeDef "Sample" (fTypeExp "String" "." [] [])]
        it "parse empty endpoint def" $ do
            -- endpoint "/" GET {}
            let tokens = [Endpoint, DQString "/", UpperId "GET", Lbra, Rbra]
            runLexerMock parse tokens `shouldBe` Right [EndpointDef "/" "GET" []]
    describe "Smopeck.Spec.Parser.parseExpr" $ do
        it "parse func call" $ do
            -- str(x)
            let tokens = [LowerId "str", Lpar, LowerId "x", Rpar]
                expected = T.Exp $ App (Func "str") [Var (Root (Absolute "x"))]
            runLexerMock parseExpr tokens `shouldBe` Right expected
        it "parse Boolean expression" $ do
            -- true == false || 0 < 1
            let tokens = [TTrue, Eq, TFalse, Or, Number 0, Lt, Number 1]
                expected = T.Exp $
                    App Syntax.Or [
                        App Syntax.Eq [Literal (LBool True), Literal (LBool False)],
                        App Syntax.Lt [Literal (LNumber 0), Literal (LNumber 1)]
                    ]
            runLexerMock parseExpr tokens `shouldBe` Right expected

    describe "Smopeck.Spec.Parser.parseTypeExp" $ do
        it "parse type synonym def" $ do
            -- Json { name : String }
            let tokens = [UpperId "Json", Lbra, LowerId "name" , Colon, UpperId "String", Rbra]
                expected = fTypeExp "Json" "." [(FieldString "name", fTypeExp "String" "." [] [])] []
            runLexerMock parseTypeExp tokens `shouldBe` Right expected
        it "field can contain upper identifier" $ do
            -- Json { Name: String }
            let tokens = [UpperId "Json", Lbra, UpperId "Name" , Colon, UpperId "String", Rbra]
                expected = fTypeExp "Json" "." [(FieldString "Name", fTypeExp "String" "." [] [])] []
            runLexerMock parseTypeExp tokens `shouldBe` Right expected
        it "field can contain figures if it is quoted" $ do
            -- Json { 'Content-Type' : String }
            let tokens = [UpperId "Json", Lbra, SQString "Content-Type" , Colon, UpperId "String", Rbra]
                expected = fTypeExp "Json" "." [(FieldString "Content-Type", fTypeExp "String" "." [] [])] []
            runLexerMock parseTypeExp tokens `shouldBe` Right expected

        it "parse regex match expression" $ do
            -- String [ . =~ r'[a-z]*' ]
            let tokens = [UpperId "String", Lsq, Dot, Match, SQRegex "[a-z]*", Rsq ]
                expected = fTypeExp "String" "." [] [ (Syntax.Match, T.Exp $ Literal $ LRegex "[a-z]*" )]
            runLexerMock parseTypeExp tokens `shouldBe` Right expected
        it "parse literal type" $ do
            -- "hoge"
            let tokens = [DQString "hoge"]
                expected = LElem $ T.LiteralType (LDQString "hoge")
            runLexerMock parseTypeExp tokens `shouldBe` Right expected
        it "parse conditional type" $ do
            -- Int ? 1 < 2
            let tokens = [UpperId "Int", Cond, Number 1, Lt, Number 2]
                e = T.Exp (App Syntax.Lt [Literal (LNumber 1), Literal (LNumber 2)])
                expected = LExt (T.HasCondF e $ fTypeExp "Int" "." [] [])
            runLexerMock parseTypeExp tokens `shouldBe` Right expected
