import qualified Data.ByteString.Char8 as BS

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

calc :: String -> String
calc [] = []
calc [s]
  | s == 'P' || s == 'D' = [s]
  | otherwise = "D"
calc (s1 : s2 : ss)
  | s1 == 'P' && (s2 == 'D' || s2 == '?') = "PD" ++ calc ss
  | s1 == 'P' = 'P' : calc (s2 : ss)
  | s1 == 'D' = 'D' : calc (s2 : ss)
  | otherwise = calc ('D' : s2 : ss)

main = do
  [t] <- getString
  putStrLn $ calc t
