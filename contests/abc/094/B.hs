import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

pairToList :: (a, a) -> [a]
pairToList (x, y) = [x, y]

main = do
  [n, m, x] <- getIntList
  bs <- getIntList
  print $ minimum (map length (pairToList (span (< x) bs)))
