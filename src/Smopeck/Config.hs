module Smopeck.Config(
    Command(..)
    , parseArgs
    , handleArgs
) where

import Options.Applicative

data Command = Mock | Test | Proxy
    deriving(Eq, Ord, Show)


mockOpts, testOpts, proxyOpts, opts :: ParserInfo Command
mockOpts = info parser flags
    where
    parser = pure Mock
    flags = progDesc "Mock mode" <> fullDesc 

testOpts = info parser flags
    where
    parser = pure Test
    flags = progDesc "Test mode" <> fullDesc 

proxyOpts = info parser flags
    where
    parser = pure Proxy
    flags = progDesc "Proxy mode" <> fullDesc 

opts = info (parser <**> helper) fullDesc
    where
    parser = hsubparser $
        command "mock" mockOpts
        <> command "test" testOpts
        <> command "proxy" proxyOpts

parseArgs :: [String] -> Maybe Command
parseArgs = getParseResult . execParserPure (prefs mempty) opts

handleArgs :: IO Command
handleArgs = execParser opts