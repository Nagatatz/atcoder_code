import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

countNum :: [Int] -> Int -> Int
countNum [] x = 0
countNum bs 0 = 0
countNum (b : bs) x
  | x == b = 1
  | x > b = 1 + countNum bs (x - b)
  | otherwise = 0

main = do
  [n, x] <- getIntList
  as <- getIntList
  let bs = sort as
  if sum bs < x
    then print $ n - 1
    else
      if sum bs == x
        then print n
        else print $ countNum bs x
