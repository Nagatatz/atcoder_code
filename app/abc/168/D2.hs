-- TLE
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine
getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

getNum :: [[Int]] -> [Int] -> [(Int, Int)]
getNum [] n = []
getNum (xs : xss) ns
  | head xs `elem` ns = (head xs, last xs) : getNum xss ns
  | last xs `elem` ns = (last xs, head xs) : getNum xss ns
  | otherwise = getNum xss ns

write table tup = do
  let (base, num) = tup
  let index = num - 1
  point <- VUM.read table index
  when (point == -1) $ VUM.write table index base

writes table (tup : tups) = do
  write table tup
  when (tups /= []) $ writes table tups

play table xss ns = do
  let tups = getNum xss ns
  writes table tups
  let ns' = map snd tups
  let xss2 = filter (\xs -> head xs `notElem` ns && last xs `notElem` ns) xss
  when (xss2 /= []) $ play table xss2 ns'

check table (n : ns) = do
  point <- VUM.read table n
  if point == -1
    then return False
    else
      if null ns
        then return True
        else check table ns

listNum table (n : ns) = do
  point <- VUM.read table n
  print point
  when (ns /= []) $ listNum table ns

main = do
  [n, m] <- getIntList
  xss <- getIntNList m
  table <- VUM.replicate n (-1) :: IO (VUM.IOVector Int)
  play table xss [1]
  b <- check table [1 .. (n -1)]
  if b
    then do
      putStrLn "Yes"
      listNum table [1 .. (n -1)]
    else putStrLn "No"
