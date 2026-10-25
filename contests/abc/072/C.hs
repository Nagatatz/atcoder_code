import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

spread table a = do
  p <- VUM.read table a
  VUM.write table a (p + 1)

initial table (a : as) = do
  when (a /= 0) $ spread table (a -1)
  spread table a
  spread table (a + 1)
  when (as /= []) $ initial table as

seek table n l k = do
  p <- VUM.read table n
  let k' = if p > k then p else k
  if n == l
    then return k'
    else seek table (n + 1) l k'

main = do
  n <- getInt
  as <- getIntList
  let l = maximum as + 2
  table <- VUM.replicate l 0 :: IO (VUM.IOVector Int)
  initial table as
  table' <- VU.freeze table
  print $ VU.maximum table'
