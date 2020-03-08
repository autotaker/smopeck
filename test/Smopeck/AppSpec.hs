{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Smopeck.AppSpec where
import           Control.Monad.Reader
import           Data.Function
import           Smopeck.App
import           Smopeck.Config
import           Test.Hspec
import           Test.HUnit

data MockService = MockService {
    mockGetCommand  :: IO Command
  , mockRunMockApp  :: MockConfig -> IO ()
  , mockRunTestApp  :: TestConfig -> IO ()
  , mockRunProxyApp :: ProxyConfig -> IO ()
}

newtype MockAppM a = MockAppM { runMockAppM :: ReaderT MockService IO a }
    deriving(Functor, Applicative, Monad, MonadIO, MonadReader MockService)

instance AppM MockAppM where
    getCommand = asks mockGetCommand >>= liftIO
    runMockApp conf = asks mockRunMockApp >>= liftIO . ((&) conf)
    runTestApp conf = asks mockRunTestApp >>= liftIO . ((&) conf)
    runProxyApp conf = asks mockRunProxyApp >>= liftIO . ((&) conf)

spec :: Spec
spec =
    describe "Smopeck.App" $ do
        let mockServiceBase = MockService {
                mockGetCommand = assertFailure "should not be called",
                mockRunMockApp = const $ assertFailure "should not be called",
                mockRunTestApp = const $ assertFailure "should not be called",
                mockRunProxyApp = const $ assertFailure "should not be called"
            }
        it "execute MockApp" $ do
            let config = MockConfig (TcpConfig "localhost" 8888) "example.spec"
                mockService = mockServiceBase {
                    mockGetCommand = pure (Mock config),
                    mockRunMockApp = \conf -> conf `shouldBe` config
                }
            runReaderT (runMockAppM main) mockService
        it "execute TestApp" $ do
            let config = TestConfig "example.spec"
                mockService = mockServiceBase {
                    mockGetCommand = pure (Test config),
                    mockRunTestApp = \conf -> conf `shouldBe` config
                }
            runReaderT (runMockAppM main) mockService
        it "execute ProxyApp" $ do
            let config = ProxyConfig
                mockService = mockServiceBase {
                    mockGetCommand = pure (Proxy config),
                    mockRunProxyApp = \conf -> conf `shouldBe` config
                }
            runReaderT (runMockAppM main) mockService


