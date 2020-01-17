module Smopeck.Config(
    Command(..)
    , parseArgs
    , handleArgs
) where

import Options.Applicative

data Command = Mock | Test | Proxy
    deriving(Eq, Ord, Show)


mockOpts, testOpts, proxyOpts, opts :: ParserInfo Command
mockOpts = info parser mempty
    where
    parser = pure Mock

testOpts = info parser mempty
    where
    parser = pure Test

proxyOpts = info parser mempty
    where
    parser = pure Proxy

opts = info parser mempty
    where
    parser = subparser $
        command "mock" mockOpts
        <> command "test" testOpts
        <> command "proxy" proxyOpts

parseArgs :: [String] -> Maybe Command
parseArgs = getParseResult . execParserPure (prefs mempty) opts

handleArgs :: IO Command
handleArgs = execParser opts