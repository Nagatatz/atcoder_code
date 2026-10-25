import qualified Data.ByteString.Char8 as BS
import           Data.Maybe
import qualified Data.Vector.Unboxed   as VU

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

search :: (Int, Int) -> (Int -> Bool) -> (Int, Int)
search (l, r) f
  | l >= r = (l, r)
  | f m = search (l, m) f
  | otherwise = search (m + 1, r) f
  where
    m = (l + r) `div` 2

main = do
  [n, k] <- getIntList
  as <- getIntList
  let as' = VU.fromList as
  let x = 1000000000
  print $ fst $ search (1, x) (\t -> VU.foldl (\s a -> s + t `div` a) 0 as' >= k)
