module Smopeck.Config(
    Command(..)
    , ParseError(..)
    , parseArgs
    , handleArgs
) where

import Options.Applicative

data Command = Mock | Test | Proxy
    deriving(Eq, Ord, Show)


mockOpts :: ParserInfo Command
mockOpts = info parser mempty
    where
    parser = pure Mock

testOpts :: ParserInfo Command
testOpts = info parser mempty
    where
    parser = pure Test

proxyOpts :: ParserInfo Command
proxyOpts = info parser mempty
    where
    parser = pure Proxy

opts :: ParserInfo Command
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