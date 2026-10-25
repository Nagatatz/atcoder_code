import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

calc table order xs index count = do
  let destination = xs VU.! index - 1
  visited <- VUM.read table index
  VUM.write order count (index + 1)
  if visited /= - 1
    then return (count, visited)
    else do
      VUM.write table index count
      calc table order xs destination (count + 1)

main = do
  [n, k] <- getIntList
  as <- getIntList
  let as' = VU.fromList as
  table <- VUM.replicate n (-1) :: IO (VUM.IOVector Int)
  order <- VUM.replicate n (-1) :: IO (VUM.IOVector Int)
  (repeated, first) <- calc table order as' 0 0
  if k <= first
    then do
      result <- VUM.read order k
      print result
    else do
      let cycle = repeated - first
      let remain = (k - first) `mod` cycle
      result <- VUM.read order (first + remain)
      print result
