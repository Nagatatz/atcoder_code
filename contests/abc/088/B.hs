import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.List
import Data.Ord

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

calc :: [Int] -> Int
calc = foldr (-) 0

main = do
  n <- getInt
  xs <- getIntList
  print $ calc (sortOn Down xs)