{-# LANGUAGE TypeApplications #-}

module File (loadFileContent, ConfigurationError (..)) where

import Control.Arrow
import Control.Exception
import System.Directory

data ConfigurationError = FileDoesNotExists | FileNotReadable
  deriving (Eq, Show)

loadFileContent :: String -> IO (Either ConfigurationError String)
loadFileContent fileName = do
  fileExists <- doesFileExist fileName
  if fileExists
    then (left mapError) <$> (try @IOError $ readFile fileName)
    else pure $ Left FileDoesNotExists
  where
    mapError _ = FileNotReadable
