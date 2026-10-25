import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.Maybe

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

simulate :: Int -> [Int] -> Int -> [Int] -> Int -> [Int] -> [Int] -> [Int]
simulate x xs _ _ _ [] _ = reverse xs
simulate x xs x' xs' i (a : as) (b : bs) = simulate r rs x xs (i + 1) as bs
  where
    r' = a + x
    r'' = b + x'
    r = min r' r''
    rs = if (r' < r'') then i : xs else i : xs'

main = do
  n <- getInt
  as <- getIntList
  bs <- getIntList
  let xs = simulate (head as) [2, 1] 0 [1] 3 (tail as) bs
  print $ length xs
  putStrLn $ unwords (map show xs)
