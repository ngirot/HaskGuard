module Loader (load, LoadingError (..)) where

import Config
import Control.Arrow
import File

data LoadingError = BadConfiguration String | ConfigurationNotAccessible String

load :: IO (Either LoadingError GlobalConfiguration)
load = do
  content <- loadFileContent "config.ini"

  case content of
    Right fileContent -> left BadConfiguration <$> parseConfig fileContent
    Left FileDoesNotExists -> left (\e -> BadConfiguration e) <$> defaultConfiguration
    Left err -> pure $ Left $ ConfigurationNotAccessible ("Unable to load configuration file " ++ (show err))
