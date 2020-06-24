import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

calc a1s a2s 0 m = m
calc a1s a2s n m = calc a1s a2s (n -1) m''
  where
    m' = sum (take n a1s) + sum (drop (n -1) a2s)
    m'' = if m' > m then m' else m

main = do
  n <- getInt
  a1s <- getIntList
  a2s <- getIntList
  print $ calc a1s a2s n 0
