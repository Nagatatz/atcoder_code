import qualified Data.ByteString.Char8 as BS

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

cc :: String -> String -> Int -> Int
cc [] _ n = n
cc _ [] n = n
cc (s : ss) (c : cs) n
  | s == c = cc ss cs (n + 1)
  | otherwise = n

calc ss cs
  | ((cc ss cs 0) + (cc (reverse ss) (reverse cs) 0)) >= length cs = "YES"
  | otherwise = "NO"

main = do
  [s] <- getString
  putStrLn $ calc s "keyence"
