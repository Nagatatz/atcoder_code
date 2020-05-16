import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine
getString = BS.unpack <$> BS.getLine

judge :: String -> Int -> Int -> String
judge [] _ _ = ""
judge (s : ss) a b
  | (a == 0) && (b == 0) = "No\n" ++ judge ss a b
  | (s == 'a') && (a > 0) = "Yes\n" ++ judge ss (a -1) b
  | (s == 'a') && (b > 0) = "Yes\n" ++ judge ss a (b -1)
  | (s == 'b') && (b > 0) = "Yes\n" ++ judge ss a (b -1)
  | otherwise = "No\n" ++ judge ss a b

main = do
  [n, a, b] <- getIntList
  s <- getString
  putStrLn $ judge s a b
