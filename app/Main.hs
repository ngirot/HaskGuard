module Main (main) where

import Lib (serve)
import Loader

main :: IO ()
main = do
  loadedConf <- load
  case loadedConf of
    Right conf -> serve conf
    Left (ConfigurationNotAccessible e) -> putStrLn $ "Unable to load configuration " ++ show e
    Left (BadConfiguration e) -> putStrLn $ "Bad configuration " ++ show e
