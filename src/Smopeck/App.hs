{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Smopeck.App
    ( main
    , DefaultAppM
    , runDefaultAppM
    , AppM(..)
    )
where
import           Control.Monad.Logger
import           Control.Monad.Reader
import qualified Smopeck.App.Check    as Check
import qualified Smopeck.App.Mock     as Mock
import qualified Smopeck.App.Proxy    as Proxy
import qualified Smopeck.App.Test     as Test
import           Smopeck.Config
import           System.IO
import           TextShow

class MonadLoggerIO m  => AppM m where
    getCommand :: m Command
    runMockApp :: MockConfig -> m ()
    runTestApp :: TestConfig -> m ()
    runProxyApp :: ProxyConfig -> m ()
    runCheckApp :: CheckConfig -> m ()

newtype DefaultAppM a = DefaultAppM { runDefaultAppM :: LoggingT IO a }
    deriving(Functor, Applicative, Monad, MonadIO, MonadLogger, MonadLoggerIO)

instance AppM DefaultAppM where
    getCommand = liftIO handleArgs
    runMockApp = DefaultAppM . Mock.runApp
    runTestApp = liftIO . Test.runApp
    runProxyApp = liftIO . Proxy.runApp
    runCheckApp = DefaultAppM . Check.runApp


main :: AppM m => m ()
main = do
    config <- getCommand
    $(logInfo) ("Config: " <> showt (FromStringShow config))
    case config of
        Mock conf  -> runMockApp conf
        Test conf  -> runTestApp conf
        Proxy conf -> runProxyApp conf
        Check conf -> runCheckApp conf

