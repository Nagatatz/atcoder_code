import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

calc :: [Int] -> Int -> Int -> Int
calc [] s c
  | s == 1 = -1
  | otherwise = c
calc (a : as) s c
  | a == s = calc as (s + 1) c
  | otherwise = calc as s (c + 1)

main = do
  n <- getInt
  as <- getIntList
  print $ calc as 1 0
