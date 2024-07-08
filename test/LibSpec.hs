module LibSpec(spec) where

import Config
import Control.Concurrent
import qualified Data.ByteString as S
import Lib
import Network
import Network.Socket.ByteString (recv, sendAll)
import Test.Hspec

spec :: Spec
spec = do
  connect

launchTest :: Expectation
launchTest = do
  let configuration = ServerConfiguration "localhost" 3232
  signal <- newEmptyMVar
  _ <- forkIO $ serve configuration (putMVar signal True)
  started <- takeMVar signal
  if started
    then do
      r <- runTCPClient "localhost" "3232" $ \socket -> do
        sendAll socket $ S.pack [5, 1, 0]
        aaa <- S.unpack <$> recv socket 4096
        if aaa /= [5, 0]
          then return $ aaa `shouldBe` [5, 0]
          else do
            sendAll socket $ S.pack [5, 1, 0, 1, 216, 58, 215, 36, 0, 80]
            bbb <- S.unpack <$> recv socket 4096
            pure $ bbb `shouldBe` [5, 0, 0, 1, 216, 58, 215, 36, 0, 80]
      case r of
        Right x -> x
        Left _ -> False `shouldBe` True
    else
      started `shouldBe` True

connect :: Spec
connect =
  describe "Simple connect request" $ do
    it "Should send request" $ launchTest
