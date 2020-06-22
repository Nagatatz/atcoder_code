import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine

getRoot :: Int -> Int -> Int -> Int
getRoot n r s
  | r^2 > n = s
  | n `mod` r == 0 = getRoot n (r+1) (r + n `div` r - 2)
  | otherwise = getRoot n (r + 1) s

main = do
  n <- getInt
  print $ getRoot n 1 0
