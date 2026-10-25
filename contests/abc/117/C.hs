import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine

getIntList = readIntList <$> BS.getLine

getDiffList :: [Int] -> [Int]
getDiffList [x] = []
getDiffList (x : xs) = (head xs - x) : getDiffList xs

main = do
  [n, m] <- getIntList
  xs <- getIntList
  let xs' = sort xs
  let ds = reverse (sort (getDiffList xs'))
  print $ last xs' - head xs' - sum (take (n - 1) ds)
