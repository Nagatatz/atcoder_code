-- [Toooo late]
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine

getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

getAll :: Int -> [[String]]
getAll n = replicateM n ["A", "B", "C", "D", "E"]

calc :: [String] -> Int -> Int -> Int -> Int
calc [] n t k = -1
calc (x : xs) n t k
  | n == t = k - 1
  | x == "A" = calc xs (n * 2) t (k + 1)
  | x == "B" = calc xs (n * 3) t (k + 1)
  | x == "C" = calc xs (n * 5) t (k + 1)
  | x == "D" = calc xs (n + 1) t (k + 1)
  | x == "E" = calc xs (n - 1) t (k + 1)
  | otherwise = -1

check :: [String] -> Int -> Int -> Int -> [String]
check xs n t k =
  if index /= (-1)
    then take index xs
    else []
  where
    index = calc xs n t k

checkAll :: [[String]] -> Int -> Int -> Int -> [[String]]
checkAll [] n t k = []
checkAll (xs : xss) n t k
  | result /= [] = [result] ++ checkAll xss n t k
  | otherwise = checkAll xss n t k
  where
    result = check xs n t k

calcCost :: [String] -> Int -> Int -> Int -> Int -> Int -> Int
calcCost [] a b c d e = 0
calcCost (x : xs) a b c d e
  | x == "A" = a + calcCost xs a b c d e
  | x == "B" = b + calcCost xs a b c d e
  | x == "C" = c + calcCost xs a b c d e
  | x == "D" = d + calcCost xs a b c d e
  | x == "E" = e + calcCost xs a b c d e
  | otherwise = 0

calcCostAll :: [[String]] -> Int -> Int -> Int -> Int -> Int -> [Int]
calcCostAll [] a b c d e = []
calcCostAll (xs : xss) a b c d e = [result] ++ calcCostAll xss a b c d e
  where
    result = calcCost xs a b c d e

calcAll ps = do
  let n = ps !! 0
  let costA = ps !! 1
  let costB = ps !! 2
  let costC = ps !! 3
  let costD = ps !! 4
  let costE = costD
  let n' = floor(log (fromIntegral n)) + 1
  let xss = getAll n
  let results = checkAll xss 0 n 0
  print $ minimum (calcCostAll results costA costB costC costD costE)

calcAllAll (ps : pss) = do
  calcAll ps
  when (pss /= []) $ calcAllAll pss

main = do
  t <- getInt
  pss <- getIntNList t
  calcAllAll pss
