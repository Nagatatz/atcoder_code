import qualified Data.ByteString.Char8 as BS

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

calc [] [] = 0
calc (s:ss) (t:ts) | s == t = 0 + calc ss ts
                   | otherwise = 1 + calc ss ts

main = do
  [ss] <- getString
  [ts] <- getString
  print $ calc ss ts
