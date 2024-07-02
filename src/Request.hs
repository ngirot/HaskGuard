module Request (buildPort, buildIp, parseRequestInput, generateErrorOutput, generateRequestOutput, RequestMessage (..)) where

import Control.Arrow
import Data.Word (Word8)
import Errors (RequestError (..))
import Payload
import Protocol

buildPort :: RequestMessage -> String
buildPort = findPort

buildIp :: RequestMessage -> Either RequestError String
buildIp message = left mapError $ findIp message
  where
    mapError code = ResponseError $ generateErrorOutput message code

generateRequestOutput :: RequestMessage -> [Word8]
generateRequestOutput message = generateOutput message 0

generateErrorOutput :: RequestMessage -> Word8 -> [Word8]
generateErrorOutput message code = generateOutput message code

generateOutput :: RequestMessage -> Word8 -> [Word8]
generateOutput message code = [requestVersion message, code, 0, (requestAddressType message)] ++ (requestAddress message) ++ (requestPort message)
