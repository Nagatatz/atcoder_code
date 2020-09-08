import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine

getIntList = readIntList <$> BS.getLine

search v n n' = do
  p <- VUM.read v (n - 1)
  if p == 0
    then do
      if n == 1
        then return 0
        else search v (n - 1) n'
    else
      if p == n'
        then do
          VUM.write v (n - 1) 0
          return p
        else
          if n == 1
            then return 0
            else search v (n - 1) (n' - 1)

simulate v xs n n' = do
  x <- search v n n'
  if x == 0
    then print (-1)
    else do
      let xs' = x : xs
      if n' == 1
        then do
          mapM_ print xs'
        else simulate v xs' n (n' - 1)

main = do
  n <- getInt
  bs <- getIntList
  v <- VU.thaw (VU.fromList bs)
  simulate v [] n n
