import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

calc [] = 1
calc (a : as)
  | odd a = 1 * calc as
  | even a = 2 * calc as

main = do
  n <- getInt
  as <- getIntList
  print $ 3 ^ n - calc as
