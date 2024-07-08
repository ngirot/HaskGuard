module LibSpec (spec) where

import Config
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as S
import Data.Word (Word8)
import Lib
import Network
import Network.Socket.ByteString (recv, sendAll)
import Test.Hspec

data Communication = Communication
  { comSend :: [Word8],
    comReceived :: [Word8]
  }
  deriving (Show, Eq)

spec :: Spec
spec = do
  connect

launchTest :: [Communication] -> Expectation
launchTest communications = do
  let configuration = ServerConfiguration "localhost" 3232
  signal <- newEmptyMVar
  _ <- forkIO $ serve configuration (putMVar signal True)
  started <- takeMVar signal
  if started
    then do
      r <- runTCPClient "localhost" "3232" $ \socket -> do
        forM_ communications (reduceCommunication socket)

      case r of
        Right v -> pure v
        Left _ -> expectationFailure "Server not started"
    else started `shouldBe` True
  where
    reduceCommunication socket com = do
      sendAll socket $ S.pack (comSend com)
      received <- S.unpack <$> recv socket 4096
      received `shouldBe` comReceived com

connect :: Spec
connect =
  describe "Simple connect request" $ do
    it "Should send request" $ launchTest [Communication [5, 1, 0] [5, 0], Communication [5, 1, 0, 1, 216, 58, 215, 36, 0, 80] [5, 0, 0, 1, 216, 58, 215, 36, 0, 80]]
