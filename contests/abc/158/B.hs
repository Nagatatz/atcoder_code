import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
  [n, a, b] <- getIntList
  let loop = n `div` (a + b)
  let remain = n `mod` (a + b)
  if remain <= a
    then print $ loop * a + remain
    else print $ (loop + 1) * a
