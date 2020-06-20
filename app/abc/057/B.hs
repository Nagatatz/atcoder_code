import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine
getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

calc :: [Int] -> [[Int]] -> Int -> Int -> Int -> Int
calc xs [] i j d = j
calc xs (ys : yss) i j d
  | d' == 0 = i
  | j > 50 = calc xs yss (i + 1) i d'
  | otherwise = calc xs yss (i + 1) j' d''
  where
    [x1, y1] = xs
    [x2, y2] = ys
    d' = abs (x1 - x2) + abs (y1 - y2)
    [d'', j'] = if d' < d then [d', i] else [d, j]

calcAll (xs : xss) yss = do
  print $ calc xs yss 1 51 (10 ^ 8 + 1)
  when (xss /= []) $ calcAll xss yss

main = do
  [n, m] <- getIntList
  xss <- getIntNList n
  yss <- getIntNList m
  calcAll xss yss
