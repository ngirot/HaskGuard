module Loader (load, LoadingError (..)) where

import Config
import Control.Arrow
import Data.Ini.Config
import Data.Text
import File

data LoadingError = BadConfiguration String | ConfigurationNotAccessible String

load :: IO (Either LoadingError ServerConfiguration)
load = do
  content <- loadFileContent "config.ini"

  case content of
    Right fileContent -> pure $ left mapError $ parseIniFile (pack fileContent) configParser
    Left FileDoesNotExists -> pure $ Right defaultConfiguration
    Left e -> pure $ Left $ ConfigurationNotAccessible ("Unable to load configuration file " ++ (show e))
  where
    mapError e = BadConfiguration e
