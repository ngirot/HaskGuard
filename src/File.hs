{-# LANGUAGE TypeApplications #-}

module File (loadFileContent, ConfigurationError (..)) where

import Control.Arrow
import Control.Exception

data ConfigurationError = FileDoesNotExists
  deriving (Eq, Show)

loadFileContent :: String -> IO (Either ConfigurationError String)
loadFileContent fileName = fmap (left mapError) (try @IOError $ readFile fileName)
  where
    mapError _ = FileDoesNotExists
