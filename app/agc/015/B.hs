import Data.Array
import qualified Data.ByteString.Char8 as BS

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

calc :: Array Int Char -> Int -> Int -> Int
calc ss i n
  | i == n = n - 1
  | i == 1 = n - 1 + calc ss (i + 1) n
  | c == 'U' = (i - 1) * 2 + (n - i) + calc ss (i + 1) n
  | c == 'D' = (i - 1) + (n - i) * 2 + calc ss (i + 1) n
  where
    c = ss ! i

main = do
  [ss] <- getString
  let n = length ss
  let ss' = listArray (1, n) ss
  print $ calc ss' 1 n
