module Smopeck.Spec.RegexUtilSpec where

import           Smopeck.Spec.RegexUtil
import           Test.Hspec

spec :: Spec
spec = do
    describe "Smopeck.Spec.RegexUtil.escapeR" $ do
        it "escapes special symbols" $ do
            let input = "^.[$()*+?{\\"
                expected = RString "\\^\\.\\[\\$\\(\\)\\*\\+\\?\\{\\\\"
            escapeR input `shouldBe` expected
        it "does not escape normal symbols" $ do
            let input = "Hello w0rld }"
                expected = RString "Hello w0rld }"
            escapeR input `shouldBe` expected
    describe "Smopeck.Spec.RegexUtil.matchR" $ do
        it "return True if regex matches the whole string" $ do
            let str = "abracadabra"
                regex = RString "[abcdra]*"
            matchR str regex `shouldBe` True
        it "return False if regex does not match the whole string" $ do
            let str = "abracadabra"
                regex = RString "rac"
            matchR str regex `shouldBe` False
    describe "Smopeck.Spec.RegexUtil.joinR" $ do
        it "concat regex with `|`" $ do
            let regexList = [ RString "abra" , RString "cada" , RString "bra" ]
                expected = RString "abra|cada|bra"
            joinR regexList `shouldBe` expected
        it "returns the original if length is one" $ do
            let regexList = [ RString ".*" ]
                expected = RString ".*"
            joinR regexList `shouldBe` expected


