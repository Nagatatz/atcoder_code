import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine

getIntList = readIntList <$> BS.getLine

getMinimumIndex :: Array Int Int -> Int -> Int -> Int -> Int
getMinimumIndex num_array value idx end
  | idx > end = end + 1
  | num_array ! idx > value && idx == end = end
  | num_array ! idx >= value = idx
  | otherwise = getMinimumIndex num_array value (idx + 1) end

getMaximumIndex :: Array Int Int -> Int -> Int -> Int -> Int
getMaximumIndex num_array value idx end
  | idx > end = end + 1
  | num_array ! idx < value && idx == end = end
  | num_array ! idx >= value = idx - 1
  | otherwise = getMaximumIndex num_array value (idx + 1) end

calc :: Int -> [Int] -> [Int] -> Array Int Int -> Int -> Int -> Int -> Int -> Int
calc p _ [] la _ _ _ _ = 0
calc p [] ls la _ _ _ _ = 0
calc p (p' : ps) ls la i imin imax n
  | imin' > n = 0
  | otherwise = calc p ps (tail ls) la (i + 1) imin' imax' n + x
  where
    a = abs (p - p')
    b = p + p'
    min_index = maximum [i, imin]
    max_index = maximum [i, imax]
    imin' = getMinimumIndex la a min_index n
    imax' = getMaximumIndex la b max_index n
    x
      | imin' > n = 0
      | imax' > n = n - imin' + 1
      | otherwise = imax' - imin' + 1

calcList :: [Int] -> Array Int Int -> Int -> Int -> Int
calcList [_] la _ _ = 0
calcList (l : ls') la i n = calcList ls' la (i + 1) n + calc l ls' (tail ls') la i i i n

main = do
  n <- getInt
  ls <- getIntList
  let ls' = sort ls
  let la = listArray (1, n) ls'
  print $ calcList ls' la 3 n
