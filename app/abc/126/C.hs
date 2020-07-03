import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

calc :: Int -> Int -> Int -> Double
calc i n k
  | i > n = fromIntegral 0
  | i >= k = fromIntegral (n - i + 1)
  | otherwise = 1 / (2 ^ (fromIntegral (ceiling (logBase 2 ((fromIntegral k) / (fromIntegral i)))))) + calc (i + 1) n k

main = do
  [n, k] <- getIntList
  print $ (calc 1 n k) / (fromIntegral n)
