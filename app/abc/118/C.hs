import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine

getIntList = readIntList <$> BS.getLine

ela a b
  | a == b = a
  | a > b && ab == 0 = b
  | a > b = ela ab b
  | a < b && ba == 0 = a
  | a < b = ela a ba
  where
    ab = a `mod` b
    ba = b `mod` a

calc :: [Int] -> Int -> Int
calc [] k = k
calc (a : as) k
  | k == 1 = 1
  | k == (-1) = calc as a
  | otherwise = calc as (ela a k)

main = do
  n <- getInt
  as <- getIntList
  let as' = sort as
  print $ calc as' (-1)
