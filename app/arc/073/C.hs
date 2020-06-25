import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

paint [] t' tb s = s
paint (t : ts) t' 0 0 = paint ts t' 0 t'
paint (t : ts) t' tb s
  | t > tb + t' = paint ts t' t (s + t')
  | otherwise = paint ts t' t (s + t' - (tb + t' - t))

main = do
  [n, t] <- getIntList
  ts <- getIntList
  print $ paint ts t 0 0
