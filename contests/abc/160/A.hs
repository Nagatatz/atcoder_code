import qualified Data.ByteString.Char8 as BS

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

main = do
  [s] <- getString
  putStrLn $ if s !! 2 == s !! 3 && s !! 4 == s !! 5 then "Yes" else "No"
