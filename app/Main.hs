module Main (main) where

import Config
import Control.Concurrent (forkIO)
import qualified Data.ByteString as S
import Lib (address, negotiate, port, request)
import Loader
import Lib (address, negotiate, port, request, errorResponse, RequestError(..))
import Network
import Network.Socket.ByteString (recv, sendAll)
import Streaming (stream)
import Request

main :: IO ()
main = do
  loadedConf <- load
  case loadedConf of
    Right conf -> do
      putStrLn $ "Start listening " ++ (scListen conf) ++ ":" ++ (show $ scPort conf) ++ "..."
      runTCPServer (Just $ scListen conf) (show $ scPort conf) talk
    Left (ConfigurationNotAccessible e) -> putStrLn $ "Unable to load configuration " ++ show e
    Left (BadConfiguration e) -> putStrLn $ "Bad configuration " ++ show e
  where
    onConnect s r ss = do
      sendAll s $ S.pack r
      _ <- forkIO $ stream s ss
      stream ss s
      putStrLn ">>> Completed"
    afterRequest s r connection message = do
      print connection

      putStrLn ">>> Forward"
      clientResp <- runTCPClient (address connection) (port connection) $ (onConnect s r)
      case clientResp of
        Right _ -> putStrLn "Ok"
        Left er -> do
          putStrLn "Send error payload"
          sendAll s $ S.pack $ errorResponse message er
    afterNego s d = do
      sendAll s $ S.pack d

      putStrLn ">>> Request"
      msgRequest <- recv s 1024

      print $ S.unpack msgRequest
      let req = request $ S.unpack msgRequest
      case req of
        Right (r, connection, message) -> afterRequest s r connection message
        Left (NoResponseError err) -> putStrLn $ "Error: " ++ err
        Left (ResponseError response) -> sendAll s $ S.pack $ response
    talk s = do
      putStrLn ">>> Negotiation"
      msgNegociation <- recv s 1024
      print $ S.unpack msgNegociation

      let nego = negotiate $ S.unpack msgNegociation
      case (nego) of
        Right d -> afterNego s d
        Left (NoResponseError err) -> putStrLn $ "Error: " ++ err
        Left (ResponseError response) -> sendAll s $ S.pack $ response
-- from the "network-run" package.
