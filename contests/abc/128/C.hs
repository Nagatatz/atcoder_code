import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine
getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

createList ks i n
  | i > n = []
  | i `elem` ks = 1 : createList ks (i + 1) n
  | otherwise = 0 : createList ks (i + 1) n

calc :: [Int] -> [Int] -> Int -> Bool
calc ks xs p = ((foldr (+) 0 $ zipWith (*) ks xs) `mod` 2) == p

calcAll :: [[Int]] -> [Int] -> [Int] -> Bool
calcAll [] xs [] = True
calcAll (ks : kss) xs (p : ps)
  | (calc ks xs p) == True = calcAll kss xs ps
  | otherwise = False

judge :: [[Int]] -> [[Int]] -> [Int] -> Int -> Int
judge kss [] ps n = n
judge kss (xs : xss) ps n
  | (calcAll kss xs ps) == True = judge kss xss ps (n + 1)
  | otherwise = judge kss xss ps n

main = do
  [n, m] <- getIntList
  kss <- getIntNList m
  ps <- getIntList
  let kss' = map tail kss
  let kss'' = map (\ks -> createList ks 0 n) kss'
  let xss = replicateM n [0, 1]
  let xss' = map (\xs -> 0 : xs) xss
  print $ judge kss'' xss' ps 0
