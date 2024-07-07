module Main (main) where

import Config
import Control.Concurrent (forkIO)
import qualified Data.ByteString as S
import Loader
import Lib (negotiate, request)
import Network
import Network.Socket.ByteString (recv, sendAll)
import Streaming (stream)
import Errors

main :: IO ()
main = do
  loadedConf <- load
  case loadedConf of
    Right conf -> do
      putStrLn $ "Start listening " ++ scListen conf ++ ":" ++ show (scPort conf) ++ "..."
      runTCPServer (Just $ scListen conf) (show $ scPort conf) talk
    Left (ConfigurationNotAccessible e) -> putStrLn $ "Unable to load configuration " ++ show e
    Left (BadConfiguration e) -> putStrLn $ "Bad configuration " ++ show e
  where
    onConnect s r ss = do
      sendAll s $ S.pack r
      _ <- forkIO $ stream s ss
      stream ss s
      putStrLn ">>> Completed"
      
    afterNego s d = do
      sendAll s $ S.pack d

      putStrLn ">>> Request"
      msgRequest <- recv s 1024
      print $ S.unpack msgRequest
      req <- request (S.unpack msgRequest) (onConnect s)
      case req of
        Right _ -> putStrLn "Ok"
        Left (NoResponseError err) -> putStrLn $ "Error: " ++ err
        Left (ResponseError response) -> sendAll s $ S.pack response
    talk s = do
      putStrLn ">>> Negotiation"
      msgNegociation <- recv s 1024
      print $ S.unpack msgNegociation

      let nego = negotiate $ S.unpack msgNegociation
      case nego of
        Right d -> afterNego s d
        Left (NoResponseError err) -> putStrLn $ "Error: " ++ err
        Left (ResponseError response) -> sendAll s $ S.pack response
-- from the "network-run" package.
