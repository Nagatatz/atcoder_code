import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine

getIntList = readIntList <$> BS.getLine

calc :: [Int] -> Int -> Int
calc [] _ = 0
calc [p] i
  | p == i = 1
  | otherwise = 0
calc (p : ps) i
  | p == i = 1 + calc (p : (tail ps)) (i + 1)
  | otherwise = calc ps (i + 1)

main = do
  n <- getInt
  ps <- getIntList
  print $ calc ps 1
