import           Control.Monad
import qualified Data.ByteString.Char8       as BS
import           Data.List
import           Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

simulate i h' h'' (h : hs) table = do
  v' <- VUM.read table (i - 1)
  v'' <- VUM.read table (i - 2)
  VUM.write table i (min (v' + abs (h - h')) (v'' + abs (h - h'')))
  when (hs /= []) $ simulate (i + 1) h h' hs table

playback i (h : hs) table xs = do
  let xs' = i : xs
  if i == 1 || hs == []
    then return xs'
    else do
      c <- VUM.read table i
      c' <- VUM.read table (i -1)
      if c - c' == abs (h - head hs)
        then playback (i - 1) hs table xs'
        else playback (i - 2) (tail hs) table xs'

main = do
  n <- getInt
  hs <- getIntList
  table <- VUM.replicate (n + 1) 0 :: IO (VUM.IOVector Int)
  let h = head hs
  simulate 2 h h (tail hs) table
  xs <- playback n (reverse hs) table []
  print $ length xs
  putStrLn $ unwords (map show xs)
