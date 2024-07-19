module Main (main) where

import Config
import Data.List (intercalate, nub)
import Lib (serve)
import Loader
import Logs
import System.Log.Logger

main :: IO ()
main = do
  loadedConf <- load
  let startupConfiguration = either (\_ -> defaultConfiguration) (\x -> x) loadedConf

  initLogger (gcLog startupConfiguration)
  requestLogger <- getLogger "HaskGuard"

  case loadedConf of
    Right conf -> serve (gcApplication conf) (displayDebug requestLogger) (logStartup requestLogger)
    Left (ConfigurationNotAccessible e) -> displayError requestLogger $ "Unable to load configuration " ++ show e
    Left (BadConfiguration e) -> displayError requestLogger $ "Bad configuration " ++ show e

logStartup :: Logger -> [String] -> [String] -> IO ()
logStartup logger errors hosts = do
  mapM_ (displayError logger) $ nub $ errors
  if length hosts >= 1
    then displaySuccess logger $ "Start listening on " ++ (intercalate " and " $ decorate $ hosts) ++ "..."
    else return ()
  where
    decorate ho = map (\h -> "'" ++ h ++ "'") ho

displayError :: Logger -> String -> IO ()
displayError _ "" = return ()
displayError logger msg = do
  logL logger ERROR msg

displaySuccess :: Logger -> String -> IO ()
displaySuccess _ "" = return ()
displaySuccess logger msg = do
  logL logger NOTICE msg

displayDebug :: Logger -> String -> IO ()
displayDebug _ "" = return ()
displayDebug logger msg = do
  logL logger DEBUG msg
