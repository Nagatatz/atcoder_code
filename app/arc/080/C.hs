import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
  n <- getInt
  as <- getIntList
  let oddNum = length (filter (\x -> odd x) as)
  let fourNum = length (filter (\x -> x `mod` 4 == 0) as)
  let otherNum = n - oddNum - fourNum
  let ans
       | fourNum + 1 == oddNum && otherNum == 0 = "Yes"
       | fourNum + 1 == oddNum && otherNum > 0 = "No"
       | fourNum == oddNum && otherNum >= 0 = "Yes"
       | fourNum < oddNum = "No"
       | otherwise = "Yes"
  putStrLn ans
