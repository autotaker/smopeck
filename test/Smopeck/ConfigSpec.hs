module Smopeck.ConfigSpec where
import           Smopeck.Config
import           Test.Hspec

spec :: Spec
spec = describe "Smopeck.Config.parseArgs" $ do
    it "should handle `mock` command" $ do
        let input = ["mock", "--host", "localhost", "--port", "8888", "example.spec"]
            expected = Mock $ MockConfig (TcpConfig "localhost" 8888) "example.spec"
        (snd <$> parseArgs input) `shouldBe` Just expected
    it "should handle `check` command" $ do
        let input = ["check", "--target", "http://localhost:8888/", "example.spec"]
            expected = (Check $ CheckConfig "http://localhost:8888/" "example.spec")
        (snd <$> parseArgs input) `shouldBe` Just expected
    it "should handle `test` command" $ do
        let input = ["test", "example.spec"]
            expected = (Test $ TestConfig $ "example.spec")
        (snd <$> parseArgs input) `shouldBe` Just expected
    it "should not handle other command" $ do
        let input = ["dummy"]
        parseArgs input `shouldBe` Nothing
    it "accepts verbose flag" $ do
        let input = ["mock", "--host", "localhost", "--port", "8888", "example.spec", "--verbose"]
            expected = CommonConfig { verbose = True}
        (fst <$> parseArgs input) `shouldBe` Just expected

