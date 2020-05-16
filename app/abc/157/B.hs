import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine
getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine
getIntListN n = map readInt <$> replicateM (fromIntegral n) BS.getLine

checkNum :: [Int] -> [Int] -> [Int]
checkNum (x : xs) ys
  | elem x ys = 1 : checkNum xs ys
  | otherwise = 0 : checkNum xs ys

bingos = [[1, 0, 0, 1, 0, 0, 1, 0, 0], [0, 1, 0, 0, 1, 0, 0, 1, 0], [0, 0, 1, 0, 0, 1, 0, 0, 1], [1, 1, 1, 0, 0, 0, 0, 0, 0], [0, 0, 0, 1, 1, 1, 0, 0, 0], [0, 0, 0, 0, 0, 0, 1, 1, 1], [1, 0, 0, 0, 1, 0, 0, 0, 1], [0, 0, 1, 0, 1, 0, 1, 0, 0]]

checkBingo :: [[Int]] -> [Int] -> Int
checkBingo [] ys = 0
checkBingo (xs : xss) ys
  | foldr (+) 0 (zipWith (*) xs ys) == 3 = 1 + checkBingo xss ys
  | otherwise = checkBingo xss ys

main = do
  card <- getIntNList 3
  n <- getInt
  bs <- getIntListN n
  if n < 3 then
    putStrLn "No"
  else do
    let cardList = checkNum (concat card) bs
    let result = checkBingo bingos cardList
    if result == 0
      then putStrLn "No"
      else putStrLn "Yes"
