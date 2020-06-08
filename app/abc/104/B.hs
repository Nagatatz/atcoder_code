import qualified Data.ByteString.Char8 as BS

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

searchC :: String -> Int -> Bool
searchC [] n
  | n == 1 = True
  | otherwise = False
searchC (y : ys) n
  | y == 'C' = searchC ys (n + 1)
  | y >= 'a' && y <= 'z' = searchC ys n
  | otherwise = False

main = do
  [s] <- getString
  if head s == 'A' && head (tail s) >= 'a' && head (tail s) <= 'z' && searchC (init (tail s)) 0 && last s >= 'a' && last s <= 'z'
    then putStrLn "AC"
    else putStrLn "WA"
