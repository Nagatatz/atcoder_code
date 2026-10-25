import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine
getIntListN n = map readInt <$> replicateM (fromIntegral n) BS.getLine

getItems :: [Int] -> Int -> Int
getItems as d = foldr (\ a -> (+) ((d - 1) `div` a + 1)) 0 as

main = do
  n <- getInt
  [d, x] <- getIntList
  as <- getIntListN n
  print $ getItems as d + x
  