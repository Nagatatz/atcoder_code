import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fromIntegral . fst . fromJust . BS.readInteger

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

sim l l' r m
  | l' > r = m
  | m == (-1) = sim l (l' + 1) r m'
  | m' < m = sim l (l' + 1) r m'
  | otherwise = sim l (l' + 1) r m
  where
    m' = (l * l') `mod` 2019

simulate l r m
  | l == r = m
  | otherwise = simulate (l + 1) r m'
  where
    m' = sim l (l + 1) r m

main = do
  [l, r] <- getIntList
  if r >= l + 2019
    then print 0
    else do
      let l' = l `mod` 2019
      let r' = r `mod` 2019
      let s
            | r' == l' + 1 = (r' * l') `mod` 2019
            | l' > r' = 0
            | otherwise = simulate l' r' (-1)
      print s
