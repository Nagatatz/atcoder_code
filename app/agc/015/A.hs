import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

main = do
  [n, a, b] <- getIntList
  if a > b
    then print 0
    else do
      if n == 1
        then
          if a == b
            then print 1
            else print 0
        else print $ (b - a) * (n -2) + 1
