import qualified Data.ByteString.Char8 as BS

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

main = do
  [s] <- getString
  putStrLn $ if s == "ABC" then "ARC" else "ABC"
