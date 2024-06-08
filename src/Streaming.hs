module Streaming (stream) where

import qualified Data.ByteString as S
import Network.Socket(Socket, gracefulClose)
import Network.Socket.ByteString (recv, sendAll)

stream :: Socket -> Socket -> IO ()
stream source destination = do
  binary <- recv source 4096
  if S.null binary
    then do
      gracefulClose destination 500
    else do
      sendAll destination binary
      stream source destination
