import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.Maybe
import qualified Data.Vector.Unboxed   as VU

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

getIntListN n = map readInt <$> replicateM (fromIntegral n) BS.getLine

-- thanks for https://atcoder.jp/contests/tessoku-book/submissions/35978891
search :: (Int, Int) -> (Int -> Bool) -> (Int, Int)
search (l, r) f
  | abs (l - r) == 1 = (l, r)
  | f m = search (l, m) f
  | otherwise = search (m, r) f
  where
    m = (l + r) `div` 2

calc :: [Int] -> Int -> VU.Vector Int -> IO ()
calc (x : xs) n as = do
  print $ snd $ search (-1, n) (\i -> (as VU.! i) >= x)
  when (xs /= []) $ calc xs n as

main = do
  n <- getInt
  as <- getIntList
  q <- getInt
  xs <- getIntListN q
  let as' = VU.fromList (sort as)
  calc xs n as'
