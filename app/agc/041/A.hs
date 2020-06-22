import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

main = do
  [n, a, b] <- getIntList
  if even (b - a)
    then print $ (b - a) `div` 2
    else do
      let xl = (n - maximum [a, b])
      let xw = (minimum [a, b] - 1)
      if xl < xw
        then print $ xl + 1 + (abs (b - a) - 1) `div` 2
        else print $ xw + 1 + (abs (b - a) - 1) `div` 2
