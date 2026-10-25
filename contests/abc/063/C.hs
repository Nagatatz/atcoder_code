import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntListN n = map readInt <$> replicateM (fromIntegral n) BS.getLine

main = do
  n <- getInt
  ss <- getIntListN n
  let ssum = sum ss
  if ssum `mod` 10 /= 0
    then print ssum
    else do
      let nzs = filter (\x -> x `mod` 10 /= 0) ss
      if length nzs == 0
        then print 0
        else print $ ssum - head (sort nzs)
