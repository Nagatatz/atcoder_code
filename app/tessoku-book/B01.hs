import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.List

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

main = do
  [a, b] <- getIntList
  print $ a + b
