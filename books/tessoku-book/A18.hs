import           Control.Monad
import           Data.Array.IO
import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

makeIOArray :: Int -> Int -> IO (IOArray (Int, Int) Bool)
makeIOArray h w = newArray ((0, 0), (h, w)) False

simulate' :: Int -> Int -> Int -> Int -> IOArray (Int, Int) Bool -> IO ()
simulate' i j s a table = do
  b <- readArray table (i - 1, j)
  if b
    then do
      writeArray table (i, j) True
      if j + a <= s
        then do
          writeArray table (i, j + a) True
        else do
          return ()
    else do
      return ()
  when (j /= s) $ simulate' i (j + 1) s a table

simulate :: Int -> Int -> Int -> [Int] -> IOArray (Int, Int) Bool -> IO ()
simulate i n s (a : as) table = do
  simulate' i 0 s a table
  when (i /= n) $ simulate (i + 1) n s as table

main = do
  [n, s] <- getIntList
  as <- getIntList
  table <- makeIOArray n s
  writeArray table (0, 0) True
  simulate 1 n s as table
  b <- readArray table (n, s)
  putStrLn $ if b then "Yes" else "No"