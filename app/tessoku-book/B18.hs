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
  when b $
    do
      writeArray table (i, j) True
      when (j + a <= s) $ writeArray table (i, j + a) True
  when (j /= s) $ simulate' i (j + 1) s a table

simulate :: Int -> Int -> Int -> [Int] -> IOArray (Int, Int) Bool -> IO ()
simulate i n s (a : as) table = do
  simulate' i 0 s a table
  when (i /= n) $ simulate (i + 1) n s as table

playback :: Int -> Int -> [Int] -> [Int] -> IOArray (Int, Int) Bool -> IO [Int]
playback i j (a : as) xs table = do
  s <- readArray table (i - 1, j)
  if s
    then
      if as /= []
        then playback (i - 1) j as xs table
        else return xs
    else do
      let xs' = (i : xs)
      if as /= []
        then playback (i - 1) (j - a) as xs' table
        else return xs'

main = do
  [n, s] <- getIntList
  as <- getIntList
  table <- makeIOArray n s
  writeArray table (0, 0) True
  simulate 1 n s as table
  b <- readArray table (n, s)
  if not b
    then print (-1)
    else do
      result <- playback n s (reverse as) [] table
      print $ length result
      putStrLn $ unwords (map show result)
