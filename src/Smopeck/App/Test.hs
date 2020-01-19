module Smopeck.App.Test(
    runApp

)where

import           Smopeck.Config


runApp :: TestConfig -> IO ()
runApp config = do
    print config
    putStrLn "Hello World Test App!"
