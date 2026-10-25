import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger

readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine

getIntList = readIntList <$> BS.getLine

main = do
  k <- getInt
  [a, b] <- getIntList
  putStrLn $ if b `div` k - (a -1) `div` k == 0 then "NG" else "OK"
