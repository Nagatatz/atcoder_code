import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

main = do
  [n, m] <- getIntList
  print $ n * (n - 1) `div` 2 + m * (m - 1) `div` 2
