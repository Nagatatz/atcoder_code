import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

calc :: [Integer] -> Integer -> Integer
calc [] n = n
calc (x:xs) n | result > 10 ^18 = -1
              | otherwise = calc xs result
  where result = n * x
  
main = do
  n <- getInt
  as <- getIntList
  if 0 `elem` as
    then print 0
    else print $ calc as 1