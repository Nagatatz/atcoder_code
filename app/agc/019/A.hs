import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
import Data.Ord

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

calc :: Int -> [(Int, Int)] -> Int -> Int
calc n [] p = p
calc 0 _ p = p
calc n (m : ms) p
  | n >= fst m = calc (n `mod` (fst m)) ms (p + (n `div` (fst m)) * snd m)
  | otherwise = calc n ms p

main = do
  [q, h, s, d] <- getIntList
  n <- getInt
  let ms = [(1, q), (2, h), (4, s), (8, d)]
  let ms' = sortBy (comparing (\x -> (fromIntegral (snd x) / fromIntegral (fst x)))) ms
  print $ calc (4 * n) ms' 0
