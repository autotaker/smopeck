module Main where

import           Control.Monad.Logger
import qualified Smopeck.App          as App

main :: IO ()
main = runStdoutLoggingT (App.runDefaultAppM App.main)
