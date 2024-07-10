module Main (main) where

import Data.List (intercalate)
import Lib (serve)
import Loader

main :: IO ()
main = do
  loadedConf <- load
  case loadedConf of
    Right conf -> serve conf putStrLn onStartup
    Left (ConfigurationNotAccessible e) -> putStrLn $ "Unable to load configuration " ++ show e
    Left (BadConfiguration e) -> putStrLn $ "Bad configuration " ++ show e
  where
    decorate hosts = map (\h -> "'" ++ h ++ "'") hosts
    onStartup hosts =
      putStrLn $ "Start listening on " ++ (intercalate " and " $ decorate hosts) ++ "..."
