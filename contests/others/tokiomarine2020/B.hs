import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
  [a, v] <- getIntList
  [b, w] <- getIntList
  t <- getInt
  let d = abs (a-b)
  if d <= (v-w) * t
    then putStrLn "YES"
    else putStrLn "NO"
