import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readString = map BS.unpack . BS.words

getInt = readInt <$> BS.getLine

getString = readString <$> BS.getLine

calc :: String -> Int -> Int -> Int
calc [c] k k' = k'
calc s k k'
  | c == 'E' && c' == 'E' = calc s' (k - 1) (minimum [k', k - 1])
  | c == 'E' && c' == 'W' = calc s' k k'
  | c == 'W' && c' == 'E' = calc s' k k'
  | c == 'W' && c' == 'W' = calc s' (k + 1) k'
  where
    c = head s
    s' = tail s
    c' = head s'

main = do
  n <- getInt
  [s] <- getString
  let s' = tail s
  let k = length (filter (\x -> x == 'E') s')
  print $ calc s k k
