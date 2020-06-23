import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

calc :: Int -> Int -> Int
calc a n
  | a `mod` 2 == 0 = calc (a `div` 2) (n + 1)
  | otherwise = n

main = do
  n <- getInt
  as <- getIntList
  print $ sum (map (\x -> calc x 0) as)
