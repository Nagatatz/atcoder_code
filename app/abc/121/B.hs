import qualified Data.ByteString.Char8 as BS
import Control.Monad
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine
getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

solve :: [Int] -> [Int] -> Int -> Int
solve as bs c | foldr (+) c (zipWith (*) as bs) > 0 = 1
              | otherwise = 0
                  
solveAll :: [[Int]] -> [Int] -> Int -> Int
solveAll ass bs c = foldr (\ as -> (+) (solve as bs c)) 0 ass

main = do
  [n, m, c] <- getIntList
  bs <- getIntList
  ass <- getIntNList n
  print $ solveAll ass bs c