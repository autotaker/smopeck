module Smopeck.ConfigSpec where
import           Smopeck.Config
import           Test.Hspec

spec :: Spec
spec = describe "Smopeck.Config.parseArgs" $ do
    it "should handle `mock` command"
        $ parseArgs ["mock", "--host", "localhost", "--port", "8888", "example.spec"]
        `shouldBe` Just (Mock $ MockConfig (TcpConfig "localhost" 8888) "example.spec")
    it "should handle `test` command" $
        parseArgs ["test", "example.spec"]
            `shouldBe` Just (Test $ TestConfig $ "example.spec")
    it "should handle `proxy` command" $ parseArgs ["proxy"] `shouldBe` Just
        (Proxy ProxyConfig)
    it "should not handle other command"
        $ parseArgs ["dummy"]
        `shouldBe` Nothing
