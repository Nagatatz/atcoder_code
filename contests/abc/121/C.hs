import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
import Data.Ord

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine
getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

calc (xs : xss) m t
  | m <= num = t + price * m
  | otherwise = calc xss (m - num) (t + price * num)
  where
    price = head xs
    num = last xs

main = do
  [n, m] <- getIntList
  xss <- getIntNList n
  let xss' = sortBy (comparing head) xss
  print $ calc xss' m 0
