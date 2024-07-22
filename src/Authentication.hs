module Authentication (manageAuthentication) where

import Config
import qualified Data.ByteString.Internal as BS (w2c)
import Data.Word (Word8)
import Errors
import Payload
import UserCsv

manageAuthentication :: AuthenticationConfiguration -> (String -> IO ()) -> [Word8] -> IO (Either RequestError [Word8])
manageAuthentication authConf logger payload = do
  let message = parseUserPasswordInput payload
  case message of
    Left e -> pure $ Left $ NoResponseError e
    Right m -> do
      let user = map BS.w2c $ upUser m
      let password = map BS.w2c $ upPassword m
      let credential = Credentials user password

      if credential `elem` (aucUsers authConf)
        then do
          logger $ "=== User '" ++ user ++ "' authenticated"
          pure $ Right [upVersion m, 0]
        else do
          logger $ "=== User '" ++ user ++ "' rejected (bad username/password)"
          pure $ Left $ ResponseError [upVersion m, 2]
