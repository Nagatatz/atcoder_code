import qualified Data.ByteString.Char8 as BS

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

calcDetail :: String -> Bool
calcDetail ss = a == b
  where
    (a, b) = splitAt (length ss `div` 2) ss

calc :: String -> Int
calc [s] = 0
calc (s1 : s2 : ss)
  | odd (length (s1 : s2 : ss)) = 1 + calc (s2 : ss)
  | calcDetail (s1 : s2 : ss) = 0
  | otherwise = 2 + calc ss

main = do
  [s] <- getString
  print $ length s - (calc (tail (reverse s)) + 1)
