import qualified Data.ByteString.Char8 as BS

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

main = do
  [s] <- getString
  if ('a' <= head s) && ('z' >= head s)
    then putStrLn "a"
    else putStrLn "A"
