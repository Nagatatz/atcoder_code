import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

main = do
  [n, m] <- getIntList
  print $ (1900 * m + (n - m) * 100) * (2 ^ m)
