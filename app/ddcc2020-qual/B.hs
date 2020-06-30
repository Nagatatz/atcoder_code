import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

separate as (b : bs) asum bsum
  | abs (bsum - asum) > abs (bsum - asum - 2 * b) = separate (b : as) bs (asum + b) (bsum - b)
  | otherwise = [asum, bsum]

main = do
  n <- getInt
  as <- getIntList
  let [asum, bsum] = separate [] as 0 (sum as)
  print $ abs (asum - bsum)
