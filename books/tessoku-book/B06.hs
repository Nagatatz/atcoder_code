import           Control.Monad
import qualified Data.ByteString.Char8       as BS
import           Data.List
import           Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

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
  let win = total - diff
  let all = r - l + 1
  let lose = all - win
  if win > lose
    then putStrLn "win"
    else
      if win == lose
        then putStrLn "draw"
        else putStrLn "lose"
  when (lrs /= []) $ calcValues table lrs

main = do
  n <- getInt
  as <- getIntList
  q <- getInt
  lrs <- getIntNList q
  table <- VUM.replicate (n + 1) 0 :: IO (VUM.IOVector Int)
  setCumulativeSum table 1 0 as
  calcValues table lrs
