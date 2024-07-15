module Logs (initLogger) where

import System.Console.Pretty (Color (..), color)
import System.IO (stdout)
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Logger

initLogger :: IO ()
initLogger = do
  -- console
  consoleHdlr <- streamHandler stdout DEBUG
  let consoleFormatter = customFormatter
  let console = setFormatter consoleHdlr consoleFormatter

  -- file
  fileHdlr <- fileHandler "haskguard.log" DEBUG
  let fileFormatter = simpleLogFormatter "[$loggername::$time] $prio $msg"
  let file = setFormatter fileHdlr fileFormatter

  updateGlobalLogger rootLoggerName $ setLevel DEBUG . setHandlers [console, file]

customFormatter :: LogFormatter a
customFormatter _ (NOTICE, msg) _ = return $ color Green msg
customFormatter _ (ERROR, msg) _ = return $ color Red msg
customFormatter _ (_, msg) _ = return msg
