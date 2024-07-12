module Main (main) where

import Data.List (intercalate)
import Lib (serve)
import Loader
import System.Console.Pretty (Color (..), color)

main :: IO ()
main = do
  loadedConf <- load
  case loadedConf of
    Right conf -> serve conf displayMessage logStartup
    Left (ConfigurationNotAccessible e) -> displayError $ "Unable to load configuration " ++ show e
    Left (BadConfiguration e) -> displayError $ "Bad configuration " ++ show e

logStartup :: [String] -> [String] -> IO ()
logStartup errors hosts = do
  displayError $ intercalate " | " errors
  displaySuccess $ "Start listening on " ++ (intercalate " and " $ decorate hosts) ++ "..."
  where
    decorate ho = map (\h -> "'" ++ h ++ "'") ho

displayError :: String -> IO ()
displayError msg = putStrLn $ color Red msg

displaySuccess :: String -> IO ()
displaySuccess msg = putStrLn $ color Green msg

displayMessage :: String -> IO ()
displayMessage = putStrLn
