module Smopeck.ConfigSpec where
import           Smopeck.Config
import           Test.Hspec

spec :: Spec
spec = describe "Smopeck.Config.parseArgs" $ do
    it "should handle `mock` command"
        $ parseArgs ["mock", "--host", "localhost", "--port", "8888"]
        `shouldBe` Just (Mock $ MockConfig $ TcpConfig "localhost" 8888)
    it "should handle `test` command" $ parseArgs ["test"] `shouldBe` Just
        (Test TestConfig)
    it "should handle `proxy` command" $ parseArgs ["proxy"] `shouldBe` Just
        (Proxy ProxyConfig)
    it "should not handle other command"
        $ parseArgs ["dummy"]
        `shouldBe` Nothing
