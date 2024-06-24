module Request (buildPort, buildIp, parseRequestInput, generateErrorOutput, generateRequestOutput, RequestMessage (..)) where

import qualified Data.ByteString.Internal as BS (w2c)
import Data.List (intercalate)
import Data.Word (Word8)
import Errors (RequestError (..))
import Numeric (showHex)
import Payload


buildPort :: RequestMessage -> String
buildPort message = show $ (strong * 256) + weak
  where
    weak = fromIntegral $ p !! 1
    strong = fromIntegral $ p !! 0
    p = requestPort message

buildIp :: RequestMessage -> Either RequestError String
buildIp message = case (requestAddressType message) of
  1 -> Right $ intercalate "." $ map show (requestAddress message)
  4 -> Right $ intercalate ":" $ map (\x -> showHex x "") $ doubleSize $ requestAddress message
  3 -> Right $ map BS.w2c (requestAddress message)
  _ -> Left $ ResponseError $ generateErrorOutput message 8


generateRequestOutput :: RequestMessage -> [Word8]
generateRequestOutput message = generateOutput message 0

generateErrorOutput :: RequestMessage -> Word8 -> [Word8]
generateErrorOutput message code = generateOutput message code

generateOutput :: RequestMessage -> Word8 -> [Word8]
generateOutput message code = [requestVersion message, code, 0, (requestAddressType message)] ++ (requestAddress message) ++ (requestPort message)
