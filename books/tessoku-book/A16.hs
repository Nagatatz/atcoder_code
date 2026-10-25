import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.Maybe

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

simulate :: Int -> Int -> [Int] -> [Int] -> Int
simulate x x' (a : as) (b : bs)
  | as == [] = r
  | otherwise = simulate r x as bs
  where
    r = min (a + x) (b + x')

main = do
  n <- getInt
  as <- getIntList
  bs <- getIntList
  print $ simulate (head as) 0 (tail as) bs
