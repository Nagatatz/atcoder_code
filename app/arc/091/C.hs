import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

main = do
  [n, m] <- getIntList
  let [n', m'] = sort [n, m]
  print $ abs ((n' - 2) * (m' - 2))
