import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine

getIntList = readIntList <$> BS.getLine

main = do
  n <- getInt
  as <- getIntList
  let suma = sum as
  let pm = if suma > 0 then 1 else (-1)
  let k = (abs suma) `div` n
  let sumK = sum (map (\a -> (a - pm * k) ^ 2) as)
  let sumK' = sum (map (\a -> (a - pm * (k + 1)) ^ 2) as)
  print $ minimum [sumK, sumK']
