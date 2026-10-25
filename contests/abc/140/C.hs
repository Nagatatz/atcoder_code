import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

getA :: [Int] -> Int
getA [] = 0
getA [x] = x
getA (x : y : xs) = minimum [x, y] + getA (y : xs)

getA' :: [Int] -> Int
getA' xs = head xs + getA xs

main = do
  n <- getInt
  bs <- getIntList
  print $ getA' bs
