import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine

getIntList = readIntList <$> BS.getLine

calc [] _ = 0
calc (a : as) i
  | odd i && odd a = 1 + calc as (i + 1)
  | otherwise = calc as (i + 1)

main = do
  n <- getInt
  as <- getIntList
  print $ calc as 1
