module Smopeck.Spec.LexerSpec where

import           Smopeck.Spec.Lexer
import           Test.Hspec

parse :: String -> Either String [Token]
parse str = runAlex str loop
    where
    loop :: Alex [Token]
    loop = do
        tok <- alexMonadScan
        case tok of
            EOF -> pure []
            _   -> (tok:) <$> loop

spec :: Spec
spec =
    describe "Smopeck.Spec.Parser" $ do
        it "lex type synonym def" $ do
            let tokens = [Type, UpperId "Sample", Eq, UpperId "String"]
            parse "type Sample = String" `shouldBe` Right tokens
        it "lex `type` as a reserved token only if it appears at the beginning of a line" $ do
            parse "type" `shouldBe` Right [Type]
            parse " type" `shouldBe` Right [LowerId "type"]
        it "lex `endpoint` as a reserved token only if it appears at the beginning of a line" $ do
            parse "endpoint" `shouldBe` Right [Endpoint]
            parse" endpoint" `shouldBe` Right [LowerId "endpoint"]

        it "lex empty endpoint def" $ do
            let tokens = [Endpoint, DQString "/", UpperId "GET", Lbra, Rbra]
            parse "endpoint \"/\" GET {}" `shouldBe` Right tokens

        it ("double quote escaping " ++ show '"') $
            runAlex "\" \\\" \"" alexMonadScan `shouldBe` Right (DQString " \" ")
        it ("double quote escaping " ++ show '\\') $
            runAlex "\" \\\\ \"" alexMonadScan `shouldBe` Right (DQString " \\ ")
        it ("double quote escaping " ++ show "\\d") $
            runAlex "\" \\d \"" alexMonadScan `shouldBe` Right (DQString " \\d ")

        it "lex match operator" $ do
            let input = "'abracadabra' =~ r'[abcdr]*' "
                expected = [SQString "abracadabra", Match, SQRegex "[abcdr]*" ]
            parse input `shouldBe` Right expected
        it "lex `true` as a reserved token" $
            parse "true" `shouldBe` Right [TTrue]
        it "lex `false` as a reserved token" $
            parse "false" `shouldBe` Right [TFalse]
        it "lex `||` as a logical or operator token" $
            parse "||" `shouldBe` Right [Or]
        it "lex `&&` as a logical and operator token" $
            parse "&&" `shouldBe` Right [And]



