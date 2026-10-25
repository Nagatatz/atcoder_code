import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

calc :: Int -> [Int] -> Int
calc a [] = -1
calc a (b : bs)
  | floor (((fromIntegral (b :: Int)) :: Float) * 1.08 :: Float) - b == a = b
  | otherwise = calc a bs

main = do
  [a, b] <- getIntList
  let bs = [b * 10 .. (b + 1) * 10 -1]
  print $ calc a bs
