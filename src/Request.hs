module Request where

import qualified Data.ByteString as S

data RequestMessage = RequestMessage

parseRequestInput :: S.ByteString -> RequestMessage
parseRequestInput payload = RequestMessage

generateRequestOutput :: RequestMessage -> S.ByteString
generateRequestOutput message = S.pack [5, 0, 0, 1, 142, 250, 178, 132, 0, 80]