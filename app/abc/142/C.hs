import qualified Data.ByteString.Char8 as BS
import Data.Function
import Data.List
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
  n <- getInt
  xs <- getIntList
  putStrLn $ unwords (map (show . fst) (sortBy (compare `on` snd) (zip xs [1 .. n])))
