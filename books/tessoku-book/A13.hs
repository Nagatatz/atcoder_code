import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.Maybe
import qualified Data.Vector.Unboxed   as VU

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

getIntListN n = map readInt <$> replicateM (fromIntegral n) BS.getLine

simulate' as a i j n k
  | d == k = (j, j - i)
  | d < k && isLast = (j, j - i)
  | d > k || isLast = (j, j - i - 1)
  | otherwise = simulate' as a i (j + 1) n k
  where
    d = as VU.! j - a
    isLast = j == n

simulate as i j n k v = do
  let a = as VU.! i
  let (j', v') = simulate' as a i j n k
  let v'' = v + v'
  if i == n
    then v''
    else if j' == n
      then simulate as (i + 1) n n k v''
      else simulate as (i + 1) j' n k v''


main = do
  [n, k] <- getIntList
  as <- getIntList
  let as' = VU.fromList (sort as)
  print $ simulate as' 0 0 (n - 1) k 0
