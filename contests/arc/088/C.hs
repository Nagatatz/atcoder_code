import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

calc :: Int -> Int -> Int -> Int -> Int
calc d i j k
  | d >= i && d < j = k
  | otherwise = calc d j (2 * j) (k + 1)

main = do
  [x, y] <- getIntList
  let d = y `div` x
  print $ 1 + calc d 1 2 0
