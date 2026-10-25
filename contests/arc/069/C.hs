import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

main = do
  [n, m] <- getIntList
  let n' = minimum [n, (m `div` 2)]
  let m' = m - n' * 2
  if m' < 4
    then print n'
    else do
      let n'' = m' `div` 4
      print $ n' + n''
