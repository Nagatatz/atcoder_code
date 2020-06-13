import qualified Data.ByteString.Char8 as BS

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

main = do
  [s] <- getString
  putStrLn $ take 3 s
