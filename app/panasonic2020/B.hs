import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
  [h, w] <- getIntList
  if (h == 1) || (w == 1) then 
    print 1
  else do
    let s = h * w
    let d = s `div` 2
    let m = s `mod` 2
    if m == 0 then 
      print d
    else 
      print $ d + 1
