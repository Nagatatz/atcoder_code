import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

include :: [Int] -> Int -> Bool
include [] _ = False
include (a : as) x = (a == x) || include as x

main = do
  [n, x] <- getIntList
  as <- getIntList
  putStrLn $ if include as x then "Yes" else "No"
