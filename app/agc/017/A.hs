import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine

getIntList = readIntList <$> BS.getLine

combSum n k i
  | k > n = 0
  | k == 0 || k == n = 1 + combSum n (k + i) i
  | k > n `div` 2 = (product [(k + 1) .. n]) `div` (product [1 .. (n - k)]) + combSum n (k + i) i
  | otherwise = (product [(n - k + 1) .. n]) `div` (product [1 .. k]) + combSum n (k + i) i

main = do
  [n, p] <- getIntList
  as <- getIntList
  let as' = map (\a -> a `mod` 2) as
  let o = length (filter (== 1) as')
  let e = length as - o
  let eSum = combSum e 0 1
  let oSum
        | p == 0 = combSum o 0 2
        | p == 1 = combSum o 1 2
  print $ eSum * oSum
