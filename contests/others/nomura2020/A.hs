import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
  [h1, m1, h2, m2, k] <- getIntList
  print $ h2 * 60 + m2 - (h1 * 60 + m1 + k)
