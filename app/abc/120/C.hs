import qualified Data.ByteString.Char8 as BS

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

main = do
  [s] <- getString
  let s0 = filter (== '0') s
  let s1 = filter (== '1') s
  print $ (minimum [length s0, length s1]) * 2
