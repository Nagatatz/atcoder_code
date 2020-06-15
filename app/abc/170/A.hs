import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

search :: [Int] -> Int -> Int -> Int
search (x:xs) n i | x == n = i
                  | otherwise = search xs n (i+1)

main = do
  xs <- getIntList
  print $ search xs 0 1
