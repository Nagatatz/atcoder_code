import qualified Data.ByteString.Char8 as BS

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

main = do
  [a, b] <- getIntList
  if a <= 0 && b >= 0
    then putStrLn "Zero"
    else
      if a > 0 && b > 0
        then putStrLn "Positive"
        else
          if odd (a - b + 1)
            then putStrLn "Negative"
            else putStrLn "Positive"
