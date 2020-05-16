import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
  [a, b] <- getIntList
  if b == 1 then
    print 0
  else
    if b <= a then
      print 1
    else do
      let d = (b - a) `div` (a - 1)
      let m = (b - a) `mod` (a - 1)
      if m == 0 then
        print $ d + 1
      else
        print $ d + 2