import           Control.Monad
import           Data.Array.IO
import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

makeIOArray :: Int -> Int -> IO (IOArray (Int, Int) Int)
makeIOArray h w = newArray ((0, 0), (h, w)) (-1)

simulate' :: Int -> Int -> Int -> Int -> Int -> IOArray (Int, Int) Int -> IO ()
simulate' i j vmax w v table = do
  b <- readArray table (i - 1, j)
  when (b > -1) $
    do
      b' <- readArray table (i, j)
      when (b < b' || b' == -1) $ writeArray table (i, j) b
      b'' <- readArray table (i, j + v)
      when (b + w < b'' || b'' == -1) $
        writeArray table (i, j + v) (b + w)
  when (j /= vmax) $ simulate' i (j + 1) vmax w v table

simulate :: Int -> Int -> Int -> [[Int]] -> IOArray (Int, Int) Int -> IO ()
simulate i n vmax (wv : wvs) table = do
  simulate' i 0 vmax (head wv) (last wv) table
  when (i /= n) $ simulate (i + 1) n vmax wvs table

getResult :: Int -> Int -> IOArray (Int, Int) Int -> Int -> IO Int
getResult n i table w = do
  w' <- readArray table (n, i)
  if w' == -1 || w' > w
    then getResult n (i - 1) table w
    else return i

main = do
  [n, w] <- getIntList
  wvs <- getIntNList n
  let vmax = 1000000
  table <- makeIOArray n vmax
  writeArray table (0, 0) 0
  simulate 1 n vmax wvs table
  v <- getResult n vmax table w
  print v
