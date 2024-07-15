{-# LANGUAGE OverloadedStrings #-}

module Config (configParser, ServerConfiguration (..), LogConfiguration (..), ApplicationConfiguration (..), defaultConfiguration) where

import Data.Ini.Config
import Data.Maybe
import Data.Text (Text, pack, unpack)
import System.Log.Logger

data ApplicationConfiguration = ApplicationConfiguration
  { acServer :: ServerConfiguration,
    acLog :: LogConfiguration
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

configParser :: IniParser ApplicationConfiguration
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
  return ApplicationConfiguration {acServer = serverConfiguration, acLog = logConfiguration}

defaultConfiguration :: ApplicationConfiguration
defaultConfiguration = ApplicationConfiguration {acServer = defaultServerConfiguration, acLog = defaultLogConfiguration}

defaultServerConfiguration :: ServerConfiguration
defaultServerConfiguration = ServerConfiguration {scListen = "0.0.0.0", scPort = 3128}

defaultLogConfiguration :: LogConfiguration
defaultLogConfiguration = LogConfiguration {lcConsoleLevel = INFO, lcFile = Nothing, lcFileLevel = INFO}

priority :: Text -> Either String Priority
priority v = case v of
  "DEBUG" -> Right DEBUG
  "INFO" -> Right INFO
  "ERROR" -> Right ERROR
  _ -> Left $ show v ++ " is not a valid level (accepted are DEBUG, INFO and ERROR)"
