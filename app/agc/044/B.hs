-- [Can't complete]
import Control.Monad
import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine

getIntList = readIntList <$> BS.getLine

getIndex n x y = (x - 1) * n + (y - 1)

getPosition n x = [x `div` n + 1, x `mod` n + 1]

exist table n x y = do
  value <- VUM.read table (getIndex n x y)
  if value == -1 then 0 else 1

calcCost table n x y =
  if x == 0 || x == (n + 1) || y == 0 || y == (n + 1)
    then -1
    else minimum (calcCost table n (x -1) y, calcCost table n x (y -1), calcCost table n (x + 1) y, calcCost table n x (y + 1)) + exist table n x y

calcAllCost table costTable n' n = do
  let position = getPosition n n'
  let cost = calcCost table n (head position) (tail position)
  VUM.write costTable n' cost
  when (n < n') $ calcAllCost table costTable (n' + 1) n

play (x : xs) table costTable n result = do
  let index = x - 1
  let cost = VUM.read costTable index
  let result' = result + cost
  VUM.write table index (-1)
  calcAllCost table costTable 0 n
  if xs /= []
    then play xs table costTable n result'
    else print result'

main = do
  n <- getInt
  ps <- getIntList
  table <- VUM.replicate (n ^ 2) 0 :: IO (VUM.IOVector Int)
  costTable <- VUM.replicate (n ^ 2) 0 :: IO (VUM.IOVector Int)
  calcAllCost table costTable 0 n
  play ps table costTable n 0
