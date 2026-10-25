import           Data.Array
import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

search :: Int -> Int -> Int -> Array Int Int -> Int
search l r x as = do
  let m = (l + r) `div` 2
  if x < as ! m
    then search l (m - 1) x as
    else
      if x > as ! m
        then search (m + 1) r x as
        else m

main = do
  [n, x] <- getIntList
  as <- getIntList
  let as' = listArray (1, n) as
  print $ search 1 n x as'
