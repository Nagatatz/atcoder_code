import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

main = do
  [a, b, c, k] <- getIntList
  let diff = a - b
  print $ if odd k then (- diff) else diff
