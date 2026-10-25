import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
import Data.Ord

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

takeEven :: [Int] -> Int -> Int
takeEven as 0 = 0
takeEven (a0 : a1 : as) n = a1 + takeEven as (n -1)

main = do
  n <- getInt
  as <- getIntList
  let alen = length as
  let al = alen `div` 3
  let as' = sortOn Down as
  print $ takeEven as' al
