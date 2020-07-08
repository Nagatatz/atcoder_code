import qualified Data.ByteString.Char8 as BS

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

simulate :: String -> Int -> String
simulate [] b = ""
simulate (s : ss) b
  | s /= 'B' && b == 0 = s : simulate ss b
  | s /= 'B' && b > 0 = simulate ss (b -1)
  | s == 'B' = simulate ss (b + 1)

main = do
  [s] <- getString
  putStrLn $ reverse (simulate (reverse s) 0)
