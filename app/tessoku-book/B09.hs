import           Control.Monad
import           Data.Array.IO
import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.Maybe

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

makeIOArray :: Int -> Int -> IO (IOArray (Int, Int) Int)
makeIOArray h w = newArray ((0, 0), (h + 1, w + 1)) 0

upDown :: Int -> Int -> Bool -> IOArray (Int, Int) Int -> IO ()
upDown x y plus array = do
  val <- readArray array (x, y)
  writeArray array (x, y) (val + (if plus then 1 else (-1)))

setPoints :: [[Int]] -> IOArray (Int, Int) Int -> IO ()
setPoints ([a, b, c, d] : abcds) array = do
  upDown (a + 1) (b + 1) True array
  upDown (c + 1) (b + 1) False array
  upDown (a + 1) (d + 1) False array
  upDown (c + 1) (d + 1) True array
  when (abcds /= []) $ setPoints abcds array

setRow :: Int -> Int -> Int -> Int -> IOArray (Int, Int) Int -> IO ()
setRow h w i j array = do
  val <- readArray array (i - 1, j)
  val' <- readArray array (i, j)
  writeArray array (i, j) (val + val')
  if i /= h
    then setRow h w (i + 1) j array
    else when (j /= w) $ setRow h w 1 (j + 1) array

setColumn :: Int -> Int -> Int -> Int -> IOArray (Int, Int) Int -> IO ()
setColumn h w i j array = do
  val <- readArray array (i, j - 1)
  val' <- readArray array (i, j)
  writeArray array (i, j) (val + val')
  if j /= w
    then setColumn h w i (j + 1) array
    else when (i /= h) $ setColumn h w (i + 1) 1 array

main = do
  n <- getInt
  abcds <- getIntNList n
  let m = 1500
  array <- makeIOArray m m
  setPoints abcds array
  setRow (m + 1) (m + 1) 1 1 array
  setColumn (m + 1) (m + 1) 1 1 array
  l <- getElems array
  print $ length $ filter (> 0) l
