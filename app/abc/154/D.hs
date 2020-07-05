import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine


getMax as i k l m' m
  | l < i + k = m
  | m'' > m = getMax as (i + 1) k l m'' m''
  | otherwise = getMax as (i + 1) k l m'' m
  where
    m'' = m' - as ! i + as ! (i + k)

main = do
  [n, k] <- getIntList
  ps <- getIntList
  let ps' = listArray (1, length ps) ps
  let l = length ps
  let m = sum (take k ps)
  let a = if k == n then m else getMax ps' 1 k l m m
  print $ (fromIntegral (a + k)) / 2
