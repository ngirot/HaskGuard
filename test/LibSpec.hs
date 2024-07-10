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
  let configuration = ServerConfiguration "0.0.0.0" serverPort
  signal <- newEmptyMVar
  signal2 <- newEmptyMVar
  _ <- forkIO $ runTCPServer Nothing (show port) (putMVar signal2 True) mult2Server
  _ <- forkIO $ serve configuration (\_ -> return ()) (putMVar signal True)
  fakeTargetStarted <- takeMVar signal2
  serverStarted <- takeMVar signal
  if serverStarted && fakeTargetStarted
    then do
      r <- runTCPClient "localhost" (show serverPort) $ \socket -> do
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
    it "Should transfer data from target on 'ipv4' CONNECT" $ do
      port <- freePort
      let portInBinary = toWord8 port
      launchTest
        port
        [ Communication [5, 1, 0] [5, 0],
          Communication ([5, 1, 0, 1, 127, 0, 0, 1] ++ portInBinary) ([5, 0, 0, 1, 127, 0, 0, 1] ++ portInBinary),
          Communication [1] [2]
        ]
    -- it "Should transfer data from target on 'ipv6' CONNECT" $ do
    --   port <- freePort
    --   let portInBinary = toWord8 port
    --   launchTest
    --     port
    --     [ Communication [5, 1, 0] [5, 0],
    --       Communication ([5, 1, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1] ++ portInBinary) ([5, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1] ++ portInBinary),
    --       Communication [1] [2]
    --     ]
    it "Should transfer data from target on 'domain name' CONNECT" $ do
      port <- freePort
      let portInBinary = toWord8 port
      launchTest
        port
        [ Communication [5, 1, 0] [5, 0],
          Communication ([5, 1, 0, 3, 108, 111, 99, 97, 108, 104, 111, 115, 116] ++ portInBinary) ([5, 0, 0, 3, 108, 111, 99, 97, 108, 104, 111, 115, 116] ++ portInBinary),
          Communication [1] [2] -- payload
        ]
    it "Should send '4' as error code when 'domain name' doest not exists for CONNECT" $ do
      port <- freePort
      launchTest
        port
        [ Communication [5, 1, 0] [5, 0],
          Communication [5, 1, 0, 3, 104, 111, 115, 116, 46, 102, 97, 107, 101, 0, 80] [5, 4, 0, 3, 104, 111, 115, 116, 46, 102, 97, 107, 101, 0, 80]
        ]
    it "Should send '5' as error code on a connection refused during CONNECT" $ do
      port <- freePort
      portThatWillRefuseConnection <- freePort
      let portInBinary = toWord8 portThatWillRefuseConnection
      launchTest
        port
        [ Communication [5, 1, 0] [5, 0],
          Communication ([5, 1, 0, 1, 127, 0, 0, 1] ++ portInBinary) ([5, 5, 0, 1, 127, 0, 0, 1] ++ portInBinary)
        ]
    it "Should send '7' as error code when command" $ do
      port <- freePort
      launchTest
        port
        [ Communication [5, 1, 0] [5, 0],
          Communication [5, 4, 0, 1, 127, 0, 0, 1, 0, 80] [5, 7, 0, 1, 127, 0, 0, 1, 0, 80]
        ]
    it "Should send '8' as error code when address type is not supported for CONNECT" $ do
      port <- freePort
      launchTest
        port
        [ Communication [5, 1, 0] [5, 0],
          Communication [5, 1, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 80] [5, 8, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 80]
        ]
    it "Should reject all authenticating methods with a NO ACCEPTABLE METHODS payload" $ do
      port <- freePort
      launchTest port [Communication [5, 2, 1, 2] [5, 255]]
    it "Should reject sock4 payloads by closing connection" $ do
      port <- freePort
      let portInBinary = toWord8 port
      launchTest port [Communication ([4, 1] ++ portInBinary ++ [127, 0, 0, 1, 0]) []]
  where
    freePort = getFreePort
    toWord8 i = do
      let weak = mod i 256 :: Int
      let strong = (i - weak) `div` 256 :: Int
      [fromIntegral strong, fromIntegral weak]
