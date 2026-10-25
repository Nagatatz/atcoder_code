import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine

getIntList = readIntList <$> BS.getLine

main = do
  [n, k] <- getIntList
  as <- getIntList
  let l = length (takeWhile (/= 1) as)
  let l' = n - (l + 1)
  let k' = k - 1
  print $ (l + l' + k' - 1) `div` k'
