import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

main = do
  [n, k] <- getIntList
  ps <- getIntList
  qs <- getIntList
  putStrLn $ if any (\xs -> sum xs == k) (sequence [ps, qs]) then "Yes" else "No"
