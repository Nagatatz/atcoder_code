import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

main = do
  [d, n] <- getIntList
  if n < 100
    then print $ 100 ^ d * n
    else print $ 100 ^ d * (n + 1)
