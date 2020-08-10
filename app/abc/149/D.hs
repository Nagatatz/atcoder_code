import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

readString = map BS.unpack . BS.words

getIntList = readIntList <$> BS.getLine

getString = concat . readString <$> BS.getLine

splitK :: String -> Int -> [String]
splitK [] k = []
splitK t k = s : splitK t' k
  where
    (s, t') = splitAt k t

calc :: String -> Int -> Int -> Int -> Int
calc [] p' r' s' = 0
calc [c] p' r' s'
  | c == 'p' = p'
  | c == 'r' = r'
  | c == 's' = s'
calc t p' r' s'
  | c == 'p' = p' + calc t' p' r' s'
  | c == 'r' = r' + calc t' p' r' s'
  | c == 's' = s' + calc t' p' r' s'
  where
    c = head t
    c' = head (tail t)
    t' = if c /= c' then tail t else tail (tail t)

main = do
  [n, k] <- getIntList
  [s', p', r'] <- getIntList
  t <- getString
  let ts = transpose (splitK t k)
  print $ sum $ map (\t -> calc t p' r' s') ts
