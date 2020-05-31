-- TLE
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.List

readInt = fst . fromJust . BS.readInteger

getInt = readInt <$> BS.getLine

factors :: Integer -> [Integer]
factors n = [x | x <- [1..n], n `mod` x == 0]

factorization :: Integer -> [Integer]
factorization 1 = []
factorization x = v : factorization (x `div` v)
  where
    v = (factors x) !! 1

createTuple :: [Integer] -> [(Integer, Int)]
createTuple [] = []
createTuple xs = e : createTuple xs2
  where
    num = head xs
    (xs1, xs2) = span (==num) xs
    e = (num, length xs1)

partition_of_integer :: Int -> [[Int]]
partition_of_integer n = part_int n n [] []
  where
    part_int 0 _ xs ys = (reverse xs):ys
    part_int 1 _ xs ys = (reverse (1:xs)):ys
    part_int n 1 xs ys = (reverse (replicate n 1 ++ xs)):ys
    part_int n k xs ys =
      let ys' = part_int n (k - 1) xs ys
      in if n - k >= 0 then part_int (n - k) k (k:xs) ys' else ys'

allDifferent :: (Eq a) => [a] -> Bool
allDifferent []     = True
allDifferent (x:xs) = x `notElem` xs && allDifferent xs

checkParts :: [[Int]] -> Int
checkParts [] = 1
checkParts (x:xs) | allDifferent x = length x
                  | otherwise = checkParts xs

calc :: [(Integer, Int)] -> Int
calc [] = 0
calc (t:ts) = k + calc ts
  where
    c = snd t
    k = if c == 1
      then 1
      else checkParts(reverse (partition_of_integer c))

main = do
  n <- getInt
  if n == 1
  then print 0
  else print $ calc (createTuple (factorization n))
   
