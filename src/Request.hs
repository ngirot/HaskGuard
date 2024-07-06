module Request (buildPort, buildIp, parseRequestInput, generateRequestSuccessOutput, generateRequestErrorOutput, RequestMessage (..)) where

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
    mapError code = ResponseError $ generateRequestOutput message code

generateRequestSuccessOutput :: RequestMessage -> [Word8]
generateRequestSuccessOutput message = generateRequestOutput message 0

generateRequestErrorOutput :: RequestMessage -> Word8 -> [Word8]
generateRequestErrorOutput message code = generateRequestOutput message code
