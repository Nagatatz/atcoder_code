import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

sortPermutations :: [[Int]] -> Int -> Int -> [[Int]]
sortPermutations xs n a
  | n == a = sortBy (\x y -> compare (x !! n) (y !! n)) xs
  | otherwise = sortBy (\x y -> compare (x !! a) (y !! a)) (sortPermutations xs n (a + 1))

main = do
  n <- getInt
  ps <- getIntList
  qs <- getIntList
  let ns = permutations [1 .. n]
  let ns' = sortPermutations ns (n -1) 0
  let a = fromJust (elemIndex ps ns')
  let b = fromJust (elemIndex qs ns')
  print $ abs (a - b)
