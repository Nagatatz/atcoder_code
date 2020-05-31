-- WA
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

calc :: [Int] -> Int -> Int -> Int -> Int -> Int
calc [x] mi ma y 0
  | x == 0 = y + 1
  | otherwise = -1
calc (x : xs) (-1) (-1) 0 n = calc xs x x x (n -1)
calc (x : xs) mi ma su n
  | ((mi + 1) `div` 2 + x) > 2 ^ n = -1
  | otherwise = calc xs mi' ma' (su + ma' - fix) (n -1)
  where
    mi' = (mi + 1) `div` 2 + x
    ma' = minimum [ma + x, 2 ^ n]
    fix =
      if ma' == 2 ^ n
        then (ma - (2 ^ n - x)) `div` 2
        else 0

main = do
  n <- getInt
  as <- getIntList
  let as' = reverse as
  print $ calc as' (-1) (-1) 0 n
