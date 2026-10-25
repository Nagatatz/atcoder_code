import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

calcDistance :: Int -> Int -> Int
calcDistance x k | x < k - x = 2 * x
                 | otherwise = 2 * (k - x)

calcAllDistance :: [Int] -> Int -> Int
calcAllDistance xs k = foldr (\ x -> (+) (calcDistance x k)) 0 xs

main = do
  n <- getInt
  k <- getInt
  xs <- getIntList
  print $ calcAllDistance xs k 