import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInteger

getInt = readInt <$> BS.getLine

calc k n s
  | k > n = s
  | otherwise = calc (k + 1) n s'
  where
    d = n `div` k
    s' = s + ((d + 1) * k * d) `div` 2

main = do
  n <- getInt
  print $ calc 1 n 0
