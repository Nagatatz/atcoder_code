import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine
getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

deleteCoupon :: [[Int]] -> [[Int]]
deleteCoupon [] = []
deleteCoupon ([s, t, u]:css) = [[s, t, v]] ++ deleteCoupon remain
  where
    remain = filter (\x -> x !! 0 /= s || x !! 1 /= t) css
    greatest = sortBy (\xs ys -> compare (ys !! 2)(xs !! 2)) (filter (\x -> x !! 0 == s && x !! 1 == t) css)
    v =if null greatest then
       u
    else
       maximum [head(greatest) !! 2, u]

calc :: [Int] -> [Int] -> [[Int]] -> [Int]
calc as bs [] = []
calc as bs ([s, t, u] : css) = val : calc as bs css
  where
    val = as !! (s - 1) + bs !! (t - 1) - u

main = do
  [a, b, m] <- getIntList
  as <- getIntList
  bs <- getIntList
  css <- getIntNList m
  let css' = deleteCoupon css
  let noCoupon = minimum as + minimum bs
  print $ minimum (noCoupon : calc as bs css')
