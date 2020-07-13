import Control.Monad
import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine

getIntList = readIntList <$> BS.getLine

disp :: Int -> Int -> Int -> Int
disp a b k
  | k <= a = b
  | otherwise = a

main = do
  n <- getInt
  xs <- getIntList
  let h = n `div` 2
  let xs' = sort xs
  let xs'' = listArray (1, n) xs'
  let a = xs'' ! h
  let b = xs'' ! (h + 1)
  mapM_ (print . (disp a b)) xs
