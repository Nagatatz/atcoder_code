import           Control.Monad
import qualified Data.ByteString.Char8       as BS
import           Data.List
import           Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

simulate i (a : as) (b : bs) table = do
  x'' <- VUM.read table (i - 2)
  x' <- VUM.read table (i - 1)
  VUM.write table i (min (x'' + b) (x' + a))
  when (as /= []) $ simulate (i + 1) as bs table

playback i (a : as) table xs = do
  let xs' = i : xs
  c <- VUM.read table i
  c' <- VUM.read table (i - 1)
  if i == 1
    then return xs'
    else
      if c - c' == a
        then playback (i - 1) as table xs'
        else playback (i - 2) (tail as) table xs'

main = do
  n <- getInt
  as <- getIntList
  bs <- getIntList
  table <- VUM.replicate (n + 1) 0 :: IO (VUM.IOVector Int)
  VUM.write table 2 (head as)
  simulate 3 (tail as) bs table
  xs <- playback n (reverse (0 : as)) table []
  print $ length xs
  putStrLn $ unwords (map show xs)
