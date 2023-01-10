import           Control.Monad
import           Data.Array.IO
import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.Maybe

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

readIntList = map readInt . BS.words

getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

makeIOArray :: Int -> Int -> IO (IOArray (Int, Int) Int)
makeIOArray w h = newArray ((0, 0), (w, h)) 0

setPoints :: [[Int]] -> IOArray (Int, Int) Int -> IO ()
setPoints ([x, y] : xys) array = do
  val <- readArray array (x, y)
  writeArray array (x, y) (val + 1)
  when (xys /= []) $ setPoints xys array

setRow :: Int -> Int -> Int -> Int -> IOArray (Int, Int) Int -> IO ()
setRow w h i j array = do
  val <- readArray array ((i - 1), j)
  val' <- readArray array (i, j)
  writeArray array (i, j) (val + val')
  if i /= w
    then setRow w h (i + 1) j array
    else when (j /= h) $ setRow w h 1 (j + 1) array

setColumn :: Int -> Int -> Int -> Int -> IOArray (Int, Int) Int -> IO ()
setColumn w h i j array = do
  val <- readArray array (i, (j - 1))
  val' <- readArray array (i, j)
  writeArray array (i, j) (val + val')
  if j /= h
    then setColumn w h i (j + 1) array
    else when (i /= w) $ setColumn w h (i + 1) 1 array

calc :: [[Int]] -> IOArray (Int, Int) Int -> IO ()
calc ([a, b, c, d] : abcds) array = do
  p <- readArray array (c, d)
  p' <- readArray array (c, b - 1)
  p'' <- readArray array (a - 1, d)
  p''' <- readArray array (a - 1, b - 1)
  print $ p + p''' - p' - p''
  when (abcds /= []) $ calc abcds array

main = do
  n <- getInt
  xys <- getIntNList n
  q <- getInt
  abcds <- getIntNList q
  array <- makeIOArray 1500 1500
  setPoints xys array
  setRow 1500 1500 1 1 array
  setColumn 1500 1500 1 1 array
  calc abcds array
