import qualified Data.ByteString.Char8 as BS

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

calc :: String -> String -> Int -> String
calc ss ts 1
  | ss == ts = "Yes"
  | otherwise = "No"
calc ss ts n
  | ss == ts = "Yes"
  | otherwise = calc (last ss : init ss) ts (n -1)

main = do
  [ss] <- getString
  [ts] <- getString
  let ssl = length (ss)
  putStrLn $ calc ss ts ssl
