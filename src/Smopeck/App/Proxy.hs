module Smopeck.App.Proxy (
    runApp
)where

import           Smopeck.Config


runApp :: ProxyConfig -> IO ()
runApp config = do
    print config
    putStrLn "Hello World Proxy App!"
