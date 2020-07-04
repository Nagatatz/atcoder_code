import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
readString = map BS.unpack . BS.words

getInt = readInt <$> BS.getLine
getString = readString <$> BS.getLine

simulate as bs k n
  | as' == bs' = 2 * n - k
  | otherwise = simulate as bs (k -1) n
  where
    as' = drop (n - k) as
    bs' = take k bs

main = do
  n <- getInt
  [s] <- getString
  [t] <- getString
  print $ simulate s t n n
