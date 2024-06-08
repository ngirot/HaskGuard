module Streaming (stream) where

import qualified Data.ByteString as S
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

stream :: Socket -> Socket -> IO ()
stream source destination = do
  binary <- recv source 4096
  if S.null binary
    then putStrLn "CLOSED"
    else do
      putStrLn "Write"
      sendAll destination binary
      stream source destination
