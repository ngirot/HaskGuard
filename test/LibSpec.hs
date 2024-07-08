module LibSpec (spec) where

import Config
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as S
import Data.Word (Word8)
import Lib
import Network
import Network.Socket.ByteString (recv, sendAll)
import Network.Socket.Free
import Test.Hspec

data Communication = Communication
  { comSend :: [Word8],
    comReceived :: [Word8]
  }
  deriving (Show, Eq)

spec :: Spec
spec = do
  connect

launchTest :: Int -> [Communication] -> Expectation
launchTest port communications = do
  serverPort <- getFreePort
  let configuration = ServerConfiguration "127.0.0.1" serverPort
  signal <- newEmptyMVar
  signal2 <- newEmptyMVar
  _ <- forkIO $ runTCPServer (Just "127.0.0.1") (show port) (putMVar signal2 True) mult2Server
  _ <- forkIO $ serve configuration (\_ -> return ()) (putMVar signal True)
  fakeTargetStarted <- takeMVar signal2
  serverStarted <- takeMVar signal
  if serverStarted && fakeTargetStarted
    then do
      r <- runTCPClient "127.0.0.1" (show serverPort) $ \socket -> do
        forM_ communications (reduceCommunication socket)

      case r of
        Right v -> pure v
        Left _ -> expectationFailure "Client not started"
    else [serverStarted, fakeTargetStarted] `shouldBe` [True, True]
  where
    reduceCommunication socket com = do
      sendAll socket $ S.pack (comSend com)
      received <- S.unpack <$> recv socket 4096
      received `shouldBe` comReceived com
    mult2Server socket = do
      received <- S.unpack <$> recv socket 4096
      sendAll socket $ S.pack $ [(received !! 0) * 2]

connect :: Spec
connect =
  describe "Simple connect request" $ do
    it "Should send CONNECT request and send payload with response" $ do
      port <- freePort
      let portInBinary = toWord8 port
      launchTest
        port
        [ Communication [5, 1, 0] [5, 0], -- Negotiation
          Communication ([5, 1, 0, 1, 127, 0, 0, 1] ++ portInBinary) ([5, 0, 0, 1, 127, 0, 0, 1] ++ portInBinary), -- CONNECT on 127.0.0.1
          Communication [1] [2] -- payload
        ]
  where
    freePort = getFreePort
    toWord8 i = do
      let weak = mod i 256 :: Int
      let strong = (i - weak) `div` 256 :: Int
      [fromIntegral strong, fromIntegral weak]
