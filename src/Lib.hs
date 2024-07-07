module Lib (serve) where

import Config
import Control.Concurrent (forkIO)
import qualified Data.ByteString as S
import Data.Word (Word8)
import Errors
import Negotiation (generateNegotiationOutput2)
import Network
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Request
import Streaming (stream)

serve :: ServerConfiguration -> IO ()
serve configuration = do
  putStrLn $ "Start listening " ++ scListen configuration ++ ":" ++ show (scPort configuration) ++ "..."
  runTCPServer (Just $ scListen configuration) (show $ scPort configuration) talk
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

negotiate :: [Word8] -> Either RequestError [Word8]
negotiate payload = generateNegotiationOutput2 payload

request :: [Word8] -> ([Word8] -> Socket -> IO a) -> IO (Either RequestError a)
request payload onConnect = generateRequestOutput2 payload onConnect
