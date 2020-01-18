{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Smopeck.App.Mock
    ( runApp
    , app
    )
where
import           Smopeck.Config

import           Data.Function
import           Data.String              (fromString)
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp as Warp


runApp :: MockConfig -> IO ()
runApp MockConfig{ listenAddr = TcpConfig {..} } =
    Warp.runSettings settings app
  where
    settings =
        Warp.defaultSettings
        & Warp.setPort tcpPort
        & Warp.setHost (fromString tcpHost)

app :: Application
app req respond =
    respond
        $ responseLBS
            status200
            [("Content-Type", "text/plain")]
            "Hello World!"
