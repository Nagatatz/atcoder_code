import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

simulate :: [Int] -> Int -> Int
simulate _ 0 = 0
simulate (a:as) 1 = a
simulate (a:as) n = 2 * a + simulate as (n-2)

main = do
  n <- getInt
  as <- getIntList
  let as' = reverse (sort as)
  if n == 1
    then print 0
    else print $ head as' + simulate (tail as') (n-2)
