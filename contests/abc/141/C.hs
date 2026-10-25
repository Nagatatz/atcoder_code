import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine
getIntListN n = map readInt <$> replicateM (fromIntegral n) BS.getLine

calc table (a : as) = do
  let index = a - 1
  point <- VUM.read table index
  VUM.write table index (point + 1)
  when (as /= []) $ calc table as

result table n' n q k = do
  point <- VUM.read table n'
  let point' = k - q + point
  if point' > 0
    then putStrLn "Yes"
    else putStrLn "No"
  when (n' < n) $ result table (n' + 1) n q k

main = do
  [n, k, q] <- getIntList
  as <- getIntListN q
  table <- VUM.replicate n 0 :: IO (VUM.IOVector Int)
  calc table as
  result table 0 (n -1) q k
