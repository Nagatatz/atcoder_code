import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine
getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs

comb :: Int -> [a] -> [[a]]
comb 0 xs = [[]]
comb _ [] = []
comb n (x : xs) = [x : y | y <- comb (n -1) xs] ++ comb n xs

calc :: [[Int]] -> [[Int]] -> Int -> Int
calc [] _ n = n
calc ps [] n = 0
calc ps (xy : xys) n = calc (filter (\p -> p /= xy) ps) xys n

calcAll :: [([[Int]], Int)] -> [[Int]] -> [Int]
calcAll [] xys = []
calcAll (ps : pss) xys = calc (fst ps) xys (snd ps) : calcAll pss xys

main = do
  [n, m] <- getIntList
  if m == 0
    then print 1
    else do
      xys <- getIntNList m
      let pss = map (\xs -> (comb 2 xs, length xs)) (filter (\x -> length x > 1) (powerset [1 .. n]))
      print $ maximum (calcAll pss xys)
