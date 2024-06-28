{-# LANGUAGE OverloadedStrings #-}

module Config (configParser, ServerConfiguration (..), defaultConfiguration) where

import Data.Ini.Config
import Data.Maybe
import Data.Text (pack, unpack)

data ServerConfiguration = ServerConfiguration
  { scListen :: String,
    scPort :: Int
  }
  deriving (Eq, Show)

configParser :: IniParser ServerConfiguration
configParser = fmap (fromMaybe defaultConfiguration) $
  sectionMb "SERVER" $ do
    port <- fromMaybe (scPort defaultConfiguration) <$> fieldMbOf "port" number
    listen <- fromMaybe (pack $ scListen defaultConfiguration) <$> fieldMb "listen"
    return ServerConfiguration {scListen = unpack listen, scPort = port}

defaultConfiguration :: ServerConfiguration
defaultConfiguration = ServerConfiguration {scListen = "localhost", scPort = 3128}
