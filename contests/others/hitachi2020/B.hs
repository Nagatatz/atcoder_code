import Control.Monad
import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine
getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

calc :: Array Int Int -> Array Int Int -> [[Int]] -> [Int]
calc as bs [] = []
calc as bs ([s, t, u] : css) = val : calc as bs css
  where
    val = as ! s + bs ! t - u

main = do
  [a, b, m] <- getIntList
  as <- getIntList
  bs <- getIntList
  css <- getIntNList m
  let as' = listArray (1, length as) as
  let bs' = listArray (1, length bs) bs
  let noCoupon = minimum as + minimum bs
  print $ minimum (noCoupon : calc as' bs' css)
