{-# LANGUAGE OverloadedStrings #-}

module Config (configParser, GlobalConfiguration (..), AuthenticationConfiguration (..), ServerConfiguration (..), LogConfiguration (..), ApplicationConfiguration (..), defaultConfiguration) where

import Data.Ini.Config
import Data.Maybe
import Data.Text (Text, pack, unpack)
import System.Log.Logger

data GlobalConfiguration = GlobalConfiguration
  { gcApplication :: ApplicationConfiguration,
    gcLog :: LogConfiguration
  }
  deriving (Eq, Show)

data ApplicationConfiguration = ApplicationConfiguration
  { acServer :: ServerConfiguration,
    acAuthentication :: AuthenticationConfiguration
  }
  deriving (Eq, Show)

data ServerConfiguration = ServerConfiguration
  { scListen :: String,
    scPort :: Int
  }
  deriving (Eq, Show)

data LogConfiguration = LogConfiguration
  { lcConsoleLevel :: Priority,
    lcFile :: Maybe FilePath,
    lcFileLevel :: Priority
  }
  deriving (Eq, Show)

data AuthenticationConfiguration = AuthenticationConfiguration
  { aucNoAuthentication :: Bool,
    aucUserPassword :: Bool,
    aucUsername :: Maybe String,
    aucPassword :: Maybe String
  }
  deriving (Eq, Show)

configParser :: IniParser GlobalConfiguration
configParser = do
  serverConfiguration <- fmap (fromMaybe defaultServerConfiguration) $
    sectionMb "SERVER" $ do
      port <- fromMaybe (scPort defaultServerConfiguration) <$> fieldMbOf "port" number
      listen <- fromMaybe (pack $ scListen defaultServerConfiguration) <$> fieldMb "listen"
      return ServerConfiguration {scListen = unpack listen, scPort = port}
  logConfiguration <- fmap (fromMaybe defaultLogConfiguration) $
    sectionMb "LOG" $ do
      consoleLevel <- fromMaybe (lcConsoleLevel defaultLogConfiguration) <$> fieldMbOf "console-level" priority
      file <- fieldMb "file"
      fileLevel <- fromMaybe (lcConsoleLevel defaultLogConfiguration) <$> fieldMbOf "file-level" priority
      return LogConfiguration {lcConsoleLevel = consoleLevel, lcFile = unpack <$> file, lcFileLevel = fileLevel}
  authenticationConfiguration <- fmap (fromMaybe defaultAuthenticationConfiguration) $
    sectionMb "AUTHENTICATION" $ do
      noAuthentication <- fromMaybe (aucNoAuthentication defaultAuthenticationConfiguration) <$> fieldMbOf "no-authentication" bool
      usernamePasswordAuthentication <- fromMaybe (aucUserPassword defaultAuthenticationConfiguration) <$> fieldMbOf "user-password" bool
      username <- fieldMbOf "user" string
      password <- fieldMbOf "password" string
      return AuthenticationConfiguration {aucNoAuthentication = noAuthentication, aucUserPassword = usernamePasswordAuthentication, aucUsername = username, aucPassword = password}
  return GlobalConfiguration {gcLog = logConfiguration, gcApplication = ApplicationConfiguration {acServer = serverConfiguration, acAuthentication = authenticationConfiguration}}

defaultConfiguration :: GlobalConfiguration
defaultConfiguration = GlobalConfiguration {gcApplication = defaultApplicationConfiguration, gcLog = defaultLogConfiguration}

defaultApplicationConfiguration :: ApplicationConfiguration
defaultApplicationConfiguration = ApplicationConfiguration {acAuthentication = defaultAuthenticationConfiguration, acServer = defaultServerConfiguration}

defaultServerConfiguration :: ServerConfiguration
defaultServerConfiguration = ServerConfiguration {scListen = "0.0.0.0", scPort = 3128}

defaultLogConfiguration :: LogConfiguration
defaultLogConfiguration = LogConfiguration {lcConsoleLevel = INFO, lcFile = Nothing, lcFileLevel = INFO}

defaultAuthenticationConfiguration :: AuthenticationConfiguration
defaultAuthenticationConfiguration = AuthenticationConfiguration {aucNoAuthentication = True, aucUserPassword = False, aucUsername = Nothing, aucPassword = Nothing}

priority :: Text -> Either String Priority
priority v = case v of
  "DEBUG" -> Right DEBUG
  "INFO" -> Right INFO
  "ERROR" -> Right ERROR
  _ -> Left $ show v ++ " is not a valid level (accepted are DEBUG, INFO and ERROR)"

bool :: Text -> Either String Bool
bool v = case v of
  "True" -> Right True
  "False" -> Right False
  _ -> Left $ show v ++ " is not a valid boolean (accepted are True and False)"
