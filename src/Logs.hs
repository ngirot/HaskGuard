module Logs (initLogger) where

import Config
import System.Console.Pretty (Color (..), color)
import System.IO (stdout)
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Logger

initLogger :: LogConfiguration -> IO ()
initLogger conf = do
  fff <- case (lcFile conf) of
    Just fileName -> sequence $ file <$> [fileName]
    Nothing -> return []

  c <- console

  updateGlobalLogger rootLoggerName $ setLevel DEBUG . setHandlers ([c] ++ fff)
  where
    console = do
      handler <- streamHandler stdout (lcConsoleLevel conf)
      let logFormatter = customFormatter
      return $ setFormatter handler logFormatter
    file fileName = do
      handler <- fileHandler fileName (lcFileLevel conf)
      let logFormatter = simpleLogFormatter "[$loggername::$time] $prio $msg"
      return $ setFormatter handler logFormatter

customFormatter :: LogFormatter a
customFormatter _ (NOTICE, msg) _ = return $ color Green msg
customFormatter _ (ERROR, msg) _ = return $ color Red msg
customFormatter _ (_, msg) _ = return msg
