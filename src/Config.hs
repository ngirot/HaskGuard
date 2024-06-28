{-# LANGUAGE OverloadedStrings #-}

module Config (configParser, ServerConfiguration (..)) where

import Data.Ini.Config
import Data.Maybe
import Data.Text (Text)

data ServerConfiguration = ServerConfiguration
  { scListen :: Text,
    scPort :: Int
  }
  deriving (Eq, Show)

configParser :: IniParser ServerConfiguration
configParser = fmap (fromMaybe defaultConfiguration) $
  sectionMb "SERVER" $ do
    port <- fromMaybe (scPort defaultConfiguration) <$> fieldMbOf "port" number
    listen <- fromMaybe (scListen defaultConfiguration) <$> fieldMb "listen"
    return ServerConfiguration {scListen = listen, scPort = port}
  where
    defaultConfiguration = ServerConfiguration {scListen = "localhost", scPort = 3128}
