import qualified Data.ByteString.Char8 as BS

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

calc :: String -> Int
calc [] = 0
calc ss = k + calc ss4
  where
    len_ss = length ss
    (ss1, ss2) = span (== '<') ss
    (ss3, ss4) = span (== '>') ss2
    len_ss1 = length ss1
    len_ss3 = length ss3
    k = sigma len_ss1 + sigma len_ss3 - minimum [len_ss1, len_ss3]

sigma :: Int -> Int
sigma n = (n * (n + 1)) `div` 2

main = do
  [s] <- getString
  print $ calc s
