module Smopeck.Config
    ( Command(..)
    , MockConfig(..)
    , TestConfig(..)
    , ProxyConfig(..)
    , TcpConfig(..)
    , parseArgs
    , handleArgs
    )
where

import           Options.Applicative

data Command = Mock !MockConfig | Test !TestConfig | Proxy !ProxyConfig
    deriving(Eq, Ord, Show)

data TcpConfig = TcpConfig {
    tcpHost :: String
  , tcpPort :: Int
} deriving(Eq, Ord, Show)

data MockConfig = MockConfig {
    listenAddr      :: !TcpConfig,
    mockSmopeckFile :: !FilePath
} deriving(Eq, Ord, Show)

data TestConfig = TestConfig {
    smopeckFile :: !FilePath
}
    deriving(Eq, Ord, Show)

data ProxyConfig = ProxyConfig
    deriving(Eq, Ord, Show)


mockOpts, testOpts, proxyOpts, opts :: ParserInfo Command
mockOpts = info parser flags
  where
    parser = Mock <$> (MockConfig <$> tcpConfigP <*> strArgument (metavar "SPEC"))
    tcpConfigP =
        TcpConfig
            <$> strOption (long "host" <> short 'h' <> metavar "HOST")
            <*> option auto (long "port" <> short 'p' <> metavar "PORT")
    flags = progDesc   "Mock mode" <> fullDesc

testOpts = info parser flags
  where
    parser = Test . TestConfig <$> strArgument (metavar "SPEC")
    flags  = progDesc "Test mode" <> fullDesc

proxyOpts = info parser flags
  where
    parser = pure (Proxy ProxyConfig)
    flags  = progDesc "Proxy mode" <> fullDesc

opts = info (parser <**> helper) fullDesc
  where
    parser =
        hsubparser
            $  command "mock"  mockOpts
            <> command "test"  testOpts
            <> command "proxy" proxyOpts

parseArgs :: [String] -> Maybe Command
parseArgs = getParseResult . execParserPure (prefs mempty) opts

handleArgs :: IO Command
handleArgs = execParser opts
