module Smopeck.App (main) where
import Smopeck.Config

main :: IO ()
main = do
    config <- handleArgs
    print config 

