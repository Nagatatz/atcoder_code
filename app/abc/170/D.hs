import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

fill table (c : cs) = do
  point <- VUM.read table c
  VUM.write table c (point + 1)
  when (cs /= []) $ fill table cs

fillVector table (a : as) n = do
  point <- VUM.read table a
  if point == 0 || point == 1
    then do
      if point == 1
        then do
          VUM.write table a 2
        else do
          let diva = n `div` a
          let cs = map (\x -> x * a) [1 .. diva]
          fill table cs
      when (as /= []) $ fillVector table as n
    else do
      when (as /= []) $ fillVector table as n

seek table (a : as) xs = do
  point <- VUM.read table a
  let xs' = if point == 1 then a : xs else xs
  if as == []
    then return xs'
    else seek table as xs'

main = do
  n <- getInt
  as <- getIntList
  let as' = sort as
  let m = last as'
  table <- VUM.replicate (m + 1) 0 :: IO (VUM.IOVector Int)
  fillVector table as' m
  rs <- seek table as' []
  print $ length rs
