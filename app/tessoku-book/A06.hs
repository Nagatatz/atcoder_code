import           Control.Monad
import qualified Data.ByteString.Char8       as BS
import           Data.List
import           Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

setCumulativeSum table index sum (a : as) = do
  let sum' = sum + a
  VUM.write table index sum'
  when (as /= []) $ setCumulativeSum table (index + 1) sum' as

calcValues table ([l, r] : lrs) = do
  diff <- VUM.read table (l - 1)
  total <- VUM.read table r
  print $ total - diff
  when (lrs /= []) $ calcValues table lrs

main = do
  [n, q] <- getIntList
  as <- getIntList
  lrs <- getIntNList q
  table <- VUM.replicate (n + 1) 0 :: IO (VUM.IOVector Int)
  setCumulativeSum table 1 0 as
  calcValues table lrs
