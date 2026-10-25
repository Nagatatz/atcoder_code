import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

readIntList = map readInt . BS.words

getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

setValues table ([l, r] : lrs) = do
  lv <- VUM.read table l
  rv <- VUM.read table r
  VUM.write table l (lv + 1)
  VUM.write table r (rv - 1)
  when (lrs /= []) $ setValues table lrs

displayValues table sum index end = do
  value <- VUM.read table index
  let sum' = sum + value
  print sum'
  when (index /= end) $ displayValues table sum' (index + 1) end

main = do
  t <- getInt
  n <- getInt
  lrs <- getIntNList n
  table <- VUM.replicate (t + 1) 0 :: IO (VUM.IOVector Int)
  setValues table lrs
  displayValues table 0 0 (t - 1)
