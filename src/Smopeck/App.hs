module Smopeck.App
    ( main
    )
where
import qualified Smopeck.App.Mock as Mock
import           Smopeck.Config

main :: IO ()
main = do
    config <- handleArgs
    print config
    case config of
        Mock conf -> Mock.runApp conf
        _         -> pure ()

