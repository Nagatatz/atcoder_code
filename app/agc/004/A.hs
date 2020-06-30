import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.List

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

main = do
  ns <- getIntList
  let [a, b, c] = sort ns
  let c' = c `div` 2
  print $ a * b * (c - c' * 2)
