import Control.Monad
import qualified Data.ByteString.Char8 as BS

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

calc :: Int -> String -> Int
calc p (s1 : s2 : s3 : ss)
  | s1 == s2 = calc (p + 2) ss
  | otherwise = calc (p + 1) (s2 : s3 : ss)
calc p (s1 : s2 : [])
  | s1 == s2 = p + 1
  | otherwise = p + 2
calc p (s1 : []) = p + 1
calc p [] = p

main = do
  [cs] <- getString
  print $ calc 0 cs
