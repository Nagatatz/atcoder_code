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
simulate' i j w w' v table = do
  b <- readArray table (i - 1, j)
  if b > -1
    then do
      b' <- readArray table (i, j)
      if b > b'
        then writeArray table (i, j) b
        else return ()
      if j + w' <= w
        then do
          b'' <- readArray table (i, j + w')
          if b + v > b''
            then writeArray table (i, j + w') (b + v)
            else return ()
        else return ()
    else return ()
  when (j /= w) $ simulate' i (j + 1) w w' v table

simulate :: Int -> Int -> Int -> [[Int]] -> IOArray (Int, Int) Int -> IO ()
simulate i n w (wv : wvs) table = do
  simulate' i 0 w (head wv) (last wv) table
  when (i /= n) $ simulate (i + 1) n w wvs table

getResult :: Int -> Int -> IOArray (Int, Int) Int -> Int -> IO Int
getResult n i table v = do
  v' <- readArray table (n, i)
  let v'' = max v v'
  if i /= 1
    then getResult n (i - 1) table v''
    else return v''

main = do
  [n, w] <- getIntList
  wvs <- getIntNList n
  table <- makeIOArray n w
  writeArray table (0, 0) 0
  simulate 1 n w wvs table
  v <- getResult n w table 0
  print v
