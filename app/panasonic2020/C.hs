import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

main = do
  [a, b, c] <- getIntList
  putStrLn $ if (c - (a + b)) < 0 then "No" else if (c - (a + b)) ^ 2 - 4 * a * b > 0 then "Yes" else "No"
