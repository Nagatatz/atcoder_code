import Control.Monad
import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine
getIntListN n = map readInt <$> replicateM (fromIntegral n) BS.getLine

play table n as count = do
  let index = as ! n
  if index == 2
    then return count
    else do
      entry <- VUM.read table (index - 1)
      if entry == -1
        then do
          VUM.write table (index - 1) 1
          play table index as (count + 1)
        else return (-1)

main = do
  n <- getInt
  as <- getIntListN n
  let as' = listArray (1, length as) as
  table <- VUM.replicate n (-1) :: IO (VUM.IOVector Int)
  result <- play table 1 as' 1
  print result
