import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

check :: [Int] -> Int -> Int -> Bool
check xs n k = a >= 1 && a <= n
  where
    a = k - sum xs

main = do
  [n, k] <- getIntList
  print $ (length . filter (\x -> check x n k)) (replicateM 2 [1 .. n])
