module Payload where

import Data.Word (Word8)

doubleSize :: [Word8] -> [Int]
doubleSize content = do
  let indexed = zip [0 ..] $ map fromIntegral $ content
  let weak = map snd $ filter (odd . fst) indexed
  let strong = map snd $ filter (even . fst) indexed
  map (\a -> ((fst a)*256) + (snd a)) $ zip strong weak
