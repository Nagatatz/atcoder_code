import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List
import Data.Maybe

readString = map BS.unpack . BS.words
readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getString = readString <$> BS.getLine
getIntList = readIntList <$> BS.getLine

isIntList :: String -> Bool
isIntList = all isDigit

main = do
  [a, b] <- getIntList
  [s] <- getString
  let sa = take a s
  let remain = drop a s
  let hyphen = head remain
  let sb = tail remain
  if isIntList sa && hyphen == '-' && isIntList sb
    then putStrLn "Yes"
    else putStrLn "No"
