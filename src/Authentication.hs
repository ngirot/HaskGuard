module Authentication (manageAuthentication) where

import Config
import qualified Data.ByteString.Internal as BS (w2c)
import Data.Word (Word8)
import Errors
import Payload

manageAuthentication :: AuthenticationConfiguration -> [Word8] -> Either RequestError [Word8]
manageAuthentication authConf payload = do
  let message = parseUserPasswordInput payload
  case message of
    Left e -> Left $ NoResponseError e
    Right m ->
      if (Just $ map BS.w2c $ upUser m) == (aucUsername authConf) && (Just $ map BS.w2c $ upPassword m) == (aucPassword authConf)
        then Right [upVersion m, 0]
        else Left $ ResponseError [upVersion m, 2]
