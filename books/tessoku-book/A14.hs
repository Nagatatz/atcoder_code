import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.Maybe
import qualified Data.Vector.Unboxed   as VU

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

search' :: (Int, Int) -> (Int -> Bool) -> (Int, Int)
search' (l, r) f
  | l >= r = (l, r)
  | f m = search' (l, m) f
  | otherwise = search' (m + 1, r) f
  where
    m = (l + r) `div` 2

search i abs cds k n2 = do
  let ab = abs VU.! i
  let isTrue = k - cds VU.! fst (search' (0, n2) (\m -> (cds VU.! m) >= (k - ab))) == ab
  if isTrue == False && i /= n2
    then search (i + 1) abs cds k n2
    else isTrue

main = do
  [n, k] <- getIntList
  as <- getIntList
  bs <- getIntList
  cs <- getIntList
  ds <- getIntList
  let abs = map sum $ sequence [as, bs]
  let cds = map sum $ sequence [cs, ds]
  let abs' = VU.fromList (sort abs)
  let cds' = VU.fromList (0 : sort cds)
  putStrLn $ if search 0 abs' cds' k (n * n - 1) then "Yes" else "No"
