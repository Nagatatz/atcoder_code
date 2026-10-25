import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine
getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

distSquare :: [Int] -> [Int] -> Int
distSquare _ [] = 0
distSquare [] _ = 0
distSquare (a : as) (b : bs) = (b - a) ^ 2 + distSquare as bs

comb :: Int -> [a] -> [[a]]
comb 0 xs = [[]]
comb _ [] = []
comb n (x : xs) = [x : y | y <- comb (n -1) xs] ++ comb n xs

calc :: [[[Int]]] -> Int
calc [] = 0
calc (p : ps) = k + calc ps
  where
    [as, bs] = p
    ds2 = distSquare as bs
    k =
      if ds2 - (floor (sqrt (fromIntegral ds2))) ^ 2 == 0
        then 1
        else 0

main = do
  [n, d] <- getIntList
  xss <- getIntNList n
  let pairs = comb 2 xss
  print $ calc pairs
