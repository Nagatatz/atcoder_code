import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

calc :: [Int] -> Array Int Int -> Int -> Int -> Int
calc [] as' i k = k
calc (a : as) as' i k
  | a < i = calc as as' (i + 1) k
  | a > i && as' ! a == i = calc as as' (i + 1) (k + 1)
  | otherwise = calc as as' (i + 1) k

main = do
  n <- getInt
  as <- getIntList
  let as' = listArray (1, length as) as
  print $ calc as as' 1 0
