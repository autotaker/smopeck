module Main where

import           Control.Monad.Logger
import qualified Smopeck.App          as App
import           Smopeck.Config       as Config

main :: IO ()
main = do
    (common, command) <- Config.handleArgs
    let CommonConfig { verbose = verbose} = common
        main = (App.runDefaultAppM $ App.main command)
        filter src lvl | verbose = lvl >= LevelDebug
                       | otherwise = lvl >= LevelInfo
    runStdoutLoggingT (filterLogger filter main)
