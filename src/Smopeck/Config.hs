module Smopeck.Config
    ( Command(..)
    , MockConfig(..)
    , TestConfig(..)
    , ProxyConfig(..)
    , TcpConfig(..)
    , CheckConfig(..)
    , CommonConfig(..)
    , parseArgs
    , handleArgs
    )
where

import           Options.Applicative

data Command =
  Mock !MockConfig
  | Test !TestConfig
  | Proxy !ProxyConfig
  | Check !CheckConfig
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
} deriving(Eq, Ord, Show)

data CheckConfig = CheckConfig {
  targetURL        :: !String,
  checkSmopeckFile :: !FilePath
} deriving(Eq, Ord, Show)

data ProxyConfig = ProxyConfig
    deriving(Eq, Ord, Show)

data CommonConfig = CommonConfig {
  verbose :: !Bool
} deriving(Eq, Ord, Show)

commonOpts :: Parser CommonConfig
commonOpts = CommonConfig <$> switch (long "verbose" <> short 'v')

mockOpts, testOpts, proxyOpts :: ParserInfo Command
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

checkOpts = info parser flags
  where
    parser = Check <$> (CheckConfig <$> urlP <*> strArgument (metavar "SPEC"))
    urlP = strOption (long "target" <> short 't' <> metavar "URL")
    flags = progDesc   "Mock mode" <> fullDesc

opts :: ParserInfo (CommonConfig, Command)
opts = info (liftA2 (,) commonOpts parser <**> helper) fullDesc
  where
    parser =
        hsubparser
            $  command "mock"  mockOpts
            <> command "test"  testOpts
            <> command "proxy" proxyOpts
            <> command "check" checkOpts

parseArgs :: [String] -> Maybe (CommonConfig, Command)
parseArgs = getParseResult . execParserPure (prefs mempty) opts


handleArgs :: IO (CommonConfig, Command)
handleArgs = execParser opts
