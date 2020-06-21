import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Maybe

readInteger = fst . fromJust . BS.readInteger

getInteger = readInteger <$> BS.getLine

calc :: Integer -> String -> String
calc n s
  | n <= 26 = (chr (ord 'a' + (fromIntegral n - 1))) : s
  | otherwise = calc d' ((chr (ord 'a' + m' - 1)) : s)
  where
    m = fromIntegral (n `mod` 26)
    d = n `div` 26
    m' = if m == 0 then 26 else m
    d' = if m == 0 then (d - 1) else d

main = do
  n <- getInteger
  putStrLn $ calc n ""
