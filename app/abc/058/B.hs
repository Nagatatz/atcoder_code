import qualified Data.ByteString.Char8 as BS

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

calc :: [(Char, Char)] -> String
calc [] = ""
calc (s : ss) = fst s : snd s : calc ss

main = do
  [o] <- getString
  [e] <- getString
  let lasted
        | length o /= length e = [last o]
        | otherwise = ""
  let ss = zip o e
  putStrLn $ (calc ss) ++ lasted
