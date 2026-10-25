import Control.Monad
import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

counts (a : as) i countTable numTable = do
  p <- VUM.read countTable a
  VUM.write countTable a (p + 1)
  VUM.write numTable i a
  when (as /= []) $ counts as (i + 1) countTable numTable

calc i n s countTable = do
  p <- VUM.read countTable i
  let s' = s + p * (p -1) `div` 2
  if i == n
    then return s'
    else calc (i + 1) n s' countTable

sim i n s numTable countTable answerTable = do
  k <- VUM.read numTable i
  a <- VUM.read answerTable k
  if a == 0
    then do
      c <- VUM.read countTable k
      let a' = s - (c - 1)
      print a'
      VUM.write answerTable k a
    else print a
  when (i /= n) $ sim (i + 1) n s numTable countTable answerTable

main = do
  n <- getInt
  as <- getIntList
  answerTable <- VUM.replicate (n + 1) 0 :: IO (VUM.IOVector Int)
  countTable <- VUM.replicate (n + 1) 0 :: IO (VUM.IOVector Int)
  numTable <- VUM.replicate (n + 1) 0 :: IO (VUM.IOVector Int)
  counts as 1 countTable numTable
  s <- calc 1 n 0 countTable
  sim 1 n s numTable countTable answerTable
