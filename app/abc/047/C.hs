import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

calc :: String -> Char -> Int
calc [] _ = 0
calc (c : s) c'
  | c == c' = calc s c'
  | otherwise = 1 + calc s c

main = do
  [s] <- getString
  print $ calc s (head s)
