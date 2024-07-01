module Request (generateRequestOutput2, RequestMessage (..)) where

import Control.Arrow (left)
import qualified Data.ByteString as S
import Data.Word (Word8)
import Errors
import Network
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Payload
import Protocol

generateRequestOutput2 :: [Word8] -> (Socket -> IO ()) -> IO (Either RequestError ())
generateRequestOutput2 payload onConnect = do
  let host = input >>= generateHost
  let port = generatePort <$> input
  -- ttt <- runTCPClient <$> host <*> port <*> Right (\_ -> putStrLn "HERE")
  let ttt = cli <$> input <*> host <*> port <*> Right onConnect
  --let xxx = generateNetworkError <$> input <*> Right NameOrServiceNotKnown

  -- let vvv = left (\ne -> generateNetworkError <$> input <*> Right ne) ttt
  --case ttt of
  --  Right _ -> Left $ NoResponseError "nope"
  --  Left _ -> Left $ NoResponseError "nope"

  case ttt of
    Right x -> do
      vvv <- x
      case vvv of
        Right x1 -> pure $ Right x1
        Left e1 -> pure $ Left $ e1
    Left vb -> pure $ Left vb
  where
    -- Left $ NoResponseError "nope"

    --case ttt of
    --    Right a -> case a of
    --      Right a1 -> Right a1
    --      Left a2 -> Left a2
    --    Left _ -> Left $ NoResponseError "test"

    --case ttt of
    --  Right _ -> Nothing
    --  Left _ -> case xxx of
    --    Right e -> Just $ ResponseError e
    --    Left _ -> Nothing

    input = left (\message -> NoResponseError message) $ parseRequestInput payload
    generateCommand i = left (\code -> ResponseError $ generateRequestOutput i code) $ findCommand i
    generateHost i = left (\code -> ResponseError $ generateRequestOutput i code) $ findHost i
    generatePort i = findPort i
    generateNetworkError i err = ResponseError $ generateRequestOutput i $ findNetworkErrorCode err
    cli i host port f = left (\e -> generateNetworkError i e) <$> runTCPClient host port f
