import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger

getInt = readInt <$> BS.getLine

twoList :: [Int]
twoList = [64, 32, 16, 8, 4, 2, 1]

judge :: [Int] -> Int -> Int
judge (x:xs) n | n >= x = x
               | otherwise = judge xs n

main = do
  n <- getInt
  print $ judge twoList n