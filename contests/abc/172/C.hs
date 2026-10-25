import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

fetchBook [] _ = []
fetchBook (b : bs) k
  | b > k = []
  | otherwise = b : fetchBook bs (k - b)

calc [] bs t k m m' = m'
calc as [] t k m m' = m'
calc (a : as) bs t k m m'
  | m'' > m' = calc as bs' t' k m'' m''
  | otherwise = calc as bs' t' k m'' m'
  where
    bs'' = fetchBook bs (k - t + a)
    lenBs = length bs''
    bs' = drop lenBs bs
    m'' = (m -1) + lenBs
    t' = t - a + sum bs''

getMax :: [Integer] -> Integer -> [Integer]
getMax [] _ = []
getMax (a : as) k
  | a > k = []
  | otherwise = a : getMax as (k - a)

main = do
  [n, m, k] <- getIntList
  as <- getIntList
  bs <- getIntList
  let as' = reverse (getMax as k)
  let lenAs = length as'
  let sumAs = sum as'
  if lenAs > 0
    then print $ calc as' bs sumAs k lenAs (lenAs + length (fetchBook bs (k - sumAs)))
    else print $ length (fetchBook bs k)
