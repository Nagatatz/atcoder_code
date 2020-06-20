import Control.Monad
import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine
getIntListN n = map readInt <$> replicateM (fromIntegral n) BS.getLine

calc :: Array Int Int -> Int -> Int -> Int -> Int
calc hs i k m
  | i < k = m
  | m == 0 = 0
  | otherwise = calc hs (i -1) k m'
  where
    x = hs ! i - hs ! (i - (k -1))
    m'
      | m > x = x
      | otherwise = m

main = do
  [n, k] <- getIntList
  hs <- getIntListN n
  let hs' = sort hs
  let hs'' = listArray (1, length hs') hs'
  print $ calc hs'' (length hs'') k (10 ^ 9 + 1)
