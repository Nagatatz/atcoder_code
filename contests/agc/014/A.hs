import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

play :: Int -> Int -> Int -> Int
play a b c | (a `mod` 2 == 0) && (b `mod` 2 == 0) && (c `mod` 2 == 0) && (a == b) && (b == c) = -1
           | (a `mod` 2 == 0) && (b `mod` 2 == 0) && (c `mod` 2 == 0) = 1 + play ((b+c) `div` 2) ((a+c) `div` 2) ((a+b) `div` 2)
           | otherwise = 0

main = do
  [a, b, c] <- getIntList
  print $ play a b c