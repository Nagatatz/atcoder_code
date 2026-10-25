import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
  [a, b, c, x, y] <- getIntList
  if (a + b) < 2 * c
    then print $ a * x + b * y
    else do
      let m = minimum [x, y]
      let x' = x - m
      let y' = y - m
      let m' = maximum [x, y]
      print $ minimum [(a * x' + b * y' + c * 2 * m), (c * 2 * m')]
