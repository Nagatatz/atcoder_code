import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.Char

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

getSum :: [Int] -> Int -> Int -> Int
getSum [] a b = 0
getSum (x : xs) a b = k + getSum xs a b
  where
    s = sum (map digitToInt (show x))
    k = if s >= a && s <= b then x else 0

main = do
  [n, a, b] <- getIntList
  print $ getSum [1 .. n] a b
