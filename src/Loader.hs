module Loader (load, LoadingError (..)) where

import Config
import Control.Arrow
import Data.Ini.Config
import Data.Text
import File

data LoadingError = BadConfiguration String | ConfigurationNotAccessible String

load :: IO (Either LoadingError ApplicationConfiguration)
load = do
  content <- loadFileContent "config.ini"

  case content of
    Right fileContent -> pure $ left BadConfiguration $ parseIniFile (pack fileContent) configParser
    Left FileDoesNotExists -> pure $ Right defaultConfiguration
    Left err -> pure $ Left $ ConfigurationNotAccessible ("Unable to load configuration file " ++ (show err))
