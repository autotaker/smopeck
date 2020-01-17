module Smopeck.ConfigSpec where
import Test.Hspec
import Smopeck.Config

spec :: Spec
spec = 
    describe "Smopeck.Config.parseArgs" $ do
        it "should handle `mock` command" $ 
            parseArgs ["mock"] `shouldBe` Just Mock
        it "should handle `test` command" $
            parseArgs ["test"] `shouldBe` Just Test
        it "should handle `proxy` command" $
            parseArgs ["proxy"] `shouldBe` Just Proxy
        it "should not handle other command" $
            parseArgs ["dummy"] `shouldBe` Nothing