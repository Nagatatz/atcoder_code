import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

powerJoin :: Int -> Int -> Int -> [Int]
powerJoin b p n
  | b ^ p > n = []
  | otherwise = b ^ p : powerJoin (b + 1) p n

calc :: Int -> Int -> [Int]
calc k n
  | k > n = []
  | otherwise = powerJoin 2 k 1000 ++ calc (k + 1) n

main = do
  x <- getInt
  let ns = 1 : calc 2 9
  print $ last (takeWhile (\y -> y <= x) (sort (nub ns)))
