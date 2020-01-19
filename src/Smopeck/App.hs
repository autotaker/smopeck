{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Smopeck.App
    ( main
    , DefaultAppM
    , runDefaultAppM
    , AppM(..)
    )
where
import           Control.Monad.Reader
import qualified Smopeck.App.Mock     as Mock

import qualified Smopeck.App.Proxy    as Proxy
import qualified Smopeck.App.Test     as Test
import           Smopeck.Config

data Factory = Factory {
    mockApp  :: MockConfig -> IO ()
  , testApp  :: TestConfig -> IO ()
  , proxyApp :: ProxyConfig -> IO ()
}

defaultFactory :: Factory
defaultFactory = Factory {
    mockApp = Mock.runApp
    , testApp = \_ -> pure ()
    , proxyApp = \_ -> pure ()
}

class MonadIO m => AppM m where
    getCommand :: m Command
    runMockApp :: MockConfig -> m ()
    runTestApp :: TestConfig -> m ()
    runProxyApp :: ProxyConfig -> m ()

newtype DefaultAppM a = DefaultAppM { runDefaultAppM :: IO a }
    deriving(Functor, Applicative, Monad, MonadIO)

instance AppM DefaultAppM where
    getCommand = liftIO handleArgs
    runMockApp = liftIO . Mock.runApp
    runTestApp = liftIO . Test.runApp
    runProxyApp = liftIO . Proxy.runApp


main :: AppM m => m ()
main = do
    config <- getCommand
    liftIO $ print config
    case config of
        Mock conf  -> runMockApp conf
        Test conf  -> runTestApp conf
        Proxy conf -> runProxyApp conf

