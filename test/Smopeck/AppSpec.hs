{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Smopeck.AppSpec where
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Function
import           Smopeck.App
import           Smopeck.Config
import           Test.Hspec
import           Test.HUnit

data MockService = MockService {
  mockRunMockApp    :: MockConfig -> IO ()
  , mockRunTestApp  :: TestConfig -> IO ()
  , mockRunProxyApp :: ProxyConfig -> IO ()
  , mockRunCheckApp :: CheckConfig -> IO ()
}

newtype MockAppM a = MockAppM { runMockAppM :: ReaderT MockService (NoLoggingT IO) a }
    deriving(Functor, Applicative, Monad, MonadIO, MonadLogger, MonadLoggerIO, MonadReader MockService)

instance AppM MockAppM where
    runMockApp conf = asks mockRunMockApp >>= liftIO . ((&) conf)
    runTestApp conf = asks mockRunTestApp >>= liftIO . ((&) conf)
    runProxyApp conf = asks mockRunProxyApp >>= liftIO . ((&) conf)
    runCheckApp conf = asks mockRunCheckApp >>= liftIO . ((&) conf)

run config service = runNoLoggingT (runReaderT (runMockAppM (main config)) service)
spec :: Spec
spec =
    describe "Smopeck.App" $ do
        let mockServiceBase = MockService {
                mockRunMockApp = const $ assertFailure "should not be called",
                mockRunTestApp = const $ assertFailure "should not be called",
                mockRunProxyApp = const $ assertFailure "should not be called",
                mockRunCheckApp = const $ assertFailure "should not be called"
            }
        it "execute MockApp" $ do
            let config = MockConfig (TcpConfig "localhost" 8888) "example.spec"
                mockService = mockServiceBase {
                    mockRunMockApp = \conf -> conf `shouldBe` config
                }
            run (Mock config) mockService
        it "execute TestApp" $ do
            let config = TestConfig "example.spec"
                mockService = mockServiceBase {
                    mockRunTestApp = \conf -> conf `shouldBe` config
                }
            run (Test config) mockService
        it "execute ProxyApp" $ do
            let config = ProxyConfig
                mockService = mockServiceBase {
                    mockRunProxyApp = \conf -> conf `shouldBe` config
                }
            run (Proxy config) mockService
        it "execute CheckApp" $ do
            let config = CheckConfig "http://localhost:8888" "example.spec"
                mockService = mockServiceBase {
                    mockRunCheckApp = \conf -> conf `shouldBe` config
                }
            run (Check config) mockService


