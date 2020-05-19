import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
  n <- getInt
  ds <- getIntList
  let num = n `div` 2
  let ds' = sort ds
  let a = ds' !! (num -1)
  let b = ds' !! num
  print $ b - a
