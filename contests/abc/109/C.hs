import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine

getIntList = readIntList <$> BS.getLine

main = do
  [n, x] <- getIntList
  xs <- getIntList
  print $ foldr1 gcd (map (\y -> abs (y - x)) xs)
