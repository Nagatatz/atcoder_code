import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

makeAverage [x] = x
makeAverage (x : y : xs) = makeAverage (((x + y) / 2) : xs)

main = do
  n <- getInt
  vs <- getIntList
  let us = sort vs
  print $ makeAverage us
