import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.Maybe
import qualified Data.Vector.Unboxed   as VU

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs

search' :: (Int, Int) -> (Int -> Bool) -> (Int, Int)
search' (l, r) f
  | l >= r = (l, r)
  | f m = search' (l, m) f
  | otherwise = search' (m + 1, r) f
  where
    m = (l + r) `div` 2

search i fs ss k n1 n2 = do
  let f = fs VU.! i
  let isTrue = k - ss VU.! fst (search' (0, n2) (\m -> (ss VU.! m) >= (k - f))) == f
  if isTrue == False && i /= n1
    then search (i + 1) fs ss k n1 n2
    else isTrue

main = do
  [n, k] <- getIntList
  as <- getIntList
  let (fs, ss) = splitAt (n `div` 2) as
  let fs' = VU.fromList (sort map sum (powerset fs)))
  let ss' = VU.fromList (0 : sort (map sum (powerset ss)))
  putStrLn $ if search 0 fs' ss' k (VU.length fs' - 1) (VU.length ss' - 1) then "Yes" else "No"
