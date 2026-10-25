import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine

getIntListN n = map readInt <$> replicateM (fromIntegral n) BS.getLine

main = do
  n <- getInt
  xs <- getIntListN 5
  let x = minimum xs
  print $ (n + x - 1) `div` x + 4
