import Control.Monad
import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine

getIntList = readIntList <$> BS.getLine

getSum :: Array Int Int -> Int -> Int -> Int
getSum as i k
  | i == k = abs (as ! (k - 1))
  | otherwise = abs (as ! i - as ! (i - 1)) + getSum as (i + 1) k

removeI :: Array Int Int -> Int -> Int -> Int
removeI as s i = s - abs (as ! i - as ! (i -1)) - abs (as ! (i + 1) - as ! i) + abs (as ! (i + 1) - as ! (i - 1))

main = do
  n <- getInt
  as <- getIntList
  let as' = (0 : as) ++ [0]
  let as'' = listArray (0, n + 2) as'
  let s = getSum as'' 1 (n + 1)
  mapM_ (print . (removeI as'' s)) [1 .. n]
