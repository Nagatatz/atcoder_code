import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

-- https://qiita.com/little_Haskeller/items/614a3ae20a517c19bb1f
primes :: Integral a => [a]
primes = map fromIntegral $ [2, 3] ++ primes'
  where
    primes' = [5] ++ f 1 7 primes'
    f m s (p : ps) = [n | n <- ns, gcd m n == 1] ++ f (m * p) (p * p) ps
      where
        ns = [x + y | x <- [s, s + 6 .. p * p - 2], y <- [0, 4]]

divn :: Int -> Int -> Int
divn x a
  | divided > 0 && x `mod` a == 0 = 1 + divn divided a
  | otherwise = 0
  where
    divided = x `div` a

fac :: Int -> [Int] -> Int -> [(Int, Int)]
fac 1 _ _ = []
fac x (p : ps) n
  | p > n = [(x, 1)]
  | otherwise = tup ++ fac x' ps n
  where
    count = divn x p
    (tup, x') =
      if count == 0
        then ([], x)
        else ([(p, count)], x `div` p ^ count)

calc :: [Int] -> Int
calc [] = 0
calc (t : ts) = length (takeWhile (<= t) [i * (i + 1) `div` 2 | i <- [1 ..]]) + calc ts

main = do
  n <- getInt
  if n == 1
    then print 0
    else do
      let s = floor (sqrt (fromIntegral n))
      print $ calc (map snd (fac n primes s))
