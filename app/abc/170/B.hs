import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

main = do
  [x, y] <- getIntList
  if odd y
    then putStrLn "No"
    else do
      let minus = y - x * 2
      if minus == 0
        then putStrLn "Yes"
        else do
          if minus < 0
            then putStrLn "No"
            else do
              if minus <= 2 * x
                then putStrLn "Yes"
                else putStrLn "No"
