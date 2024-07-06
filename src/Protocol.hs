module Protocol (findNegotiationReturnCode, findPort, findIp) where

import qualified Data.ByteString.Internal as BS (w2c)
import Data.List (intercalate)
import Data.Word (Word8)
import Numeric (showHex)
import Payload

findNegotiationReturnCode :: NegotiationMessage -> Either Word8 Word8
findNegotiationReturnCode message =
  if 0 `elem` negotiationMethods message
    then Right 0
    else Left 255

findPort :: RequestMessage -> String
findPort message = show $ (strong * 256 :: Int) + weak
  where
    weak = fromIntegral $ portBytes !! 1
    strong = fromIntegral $ portBytes !! 0
    portBytes = requestPort message

findIp :: RequestMessage -> Either Word8 String
findIp message = case requestAddressType message of
  1 -> Right $ intercalate "." $ map show (requestAddress message)
  4 -> Right $ intercalate ":" $ map (\x -> showHex x "") $ doubleSize $ requestAddress message
  3 -> Right $ map BS.w2c (requestAddress message)
  _ -> Left 8
  