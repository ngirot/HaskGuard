module Main (main) where

import Config
import Control.Concurrent (forkIO)
import qualified Data.ByteString as S
import Lib (address, negotiate, port, request)
import Loader
import Lib (address, negotiate, port, request, RequestError(..))
import Network
import Network.Socket.ByteString (recv, sendAll)
import Streaming (stream)

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
    afterRequest s r connection = do
      print connection
      sendAll s $ S.pack r

      putStrLn ">>> Forward"
      runTCPClient (address connection) (port connection) $ \ss -> do
        _ <- forkIO $ stream s ss
        stream ss s
        putStrLn ">>> Completed"
    afterNego s d = do
      sendAll s $ S.pack d

      putStrLn ">>> Request"
      msgRequest <- recv s 1024
      print $ S.unpack msgRequest
      let req = request $ S.unpack msgRequest
      case req of
        Right (r, connection) -> afterRequest s r connection
        Left (NoResponseError err) -> putStr $ "Error: " ++ err
        Left (ResponseError response) -> sendAll s $ S.pack $ response
    talk s = do
      putStrLn ">>> Negotiation"
      msgNegociation <- recv s 1024
      print $ S.unpack msgNegociation

      let nego = negotiate $ S.unpack msgNegociation
      case (nego) of
        Right d -> afterNego s d
        Left err -> putStrLn $ "Error: " ++ err

-- from the "network-run" package.
