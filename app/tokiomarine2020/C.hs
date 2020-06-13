--TLE
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Unboxed as VU
import Control.Monad.ST
import Data.Foldable

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

setInitial (a : as) table i = do
  VUM.write table i a
  if as /= [] then setInitial as table (i + 1) else return ()

writeTT tt (d : ds) = do
  point <- VUM.unsafeRead tt d
  VUM.unsafeWrite tt d (point + 1)
  if ds /= [] then writeTT tt ds else return ()

tableCopy table tt i n = do
  point <- VUM.unsafeRead tt i
  VUM.unsafeWrite table i point
  if i /= n -1 then tableCopy table tt (i + 1) n else return ()

ttClear tt i n = do
  VUM.write tt i 0
  if i /= n -1 then ttClear tt (i + 1) n else return ()

cc table tt i n = do
  point <- VUM.unsafeRead table i
  let ds = [maximum [i - point, 0] .. minimum [i + point, n -1]]
  writeTT tt ds
  if i /= n -1
    then cc table tt (i + 1) n
    else do
      VUM.copy table tt
      VUM.set tt 0

calc table tt k n = do
  cc table tt 0 n
  if k /= 1 then calc table tt (k -1) n else return ()

printVUM table i n = do
  point <- VUM.unsafeRead table i
  putStr $ show point
  if i /= n -1
    then do
      putStr " "
      printVUM table (i + 1) n
    else putStr " "

main = do
  [n, k] <- getIntList
  as <- getIntList
  let as' = VU.fromList as
  table <- VUM.replicate n 0 :: IO (VUM.IOVector Int)
  tt <- VUM.replicate n 0 :: IO (VUM.IOVector Int)
  for_ [1 .. n] $ \i -> VUM.write table (i -1) (as' VU.! (i - 1))
  calc table tt k n
  printVUM table 0 n
