import           Control.Monad
import           Data.Array
import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.Maybe

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine


setRow (x:xs) xss table h w i j = do
  let table' = table // [((i, j), table ! (i, j - 1) + x)]
  if xs /= []
    then setRow xs xss table' h w i (j + 1)
    else
      if xss /= []
        then setRow (head xss) (tail xss) table' h w (i + 1) 1
        else table'

setColumn base table h w i j = do
  let table' = table // [((i, j), table ! (i - 1, j) + base ! (i, j))]
  if i /= h
    then setColumn base table' h w (i + 1) j
    else
      if j /= w
        then setColumn base table' h w 1 (j + 1)
        else table'

calc table ([a, b, c, d] : abcds) = do
  print (table ! (c, d) + table ! (a - 1, b - 1) - table ! (a - 1, d) - table ! (c, b - 1))
  when (abcds /= []) $ calc table abcds

main = do
  [h, w] <- getIntList
  xss <- getIntNList h
  q <- getInt
  abcds <- getIntNList q
  let table = listArray ((0, 0), (h, w)) (take ((h + 1) * (w + 1)) (repeat 0))
  let table' = setRow (head xss) (tail xss) table h w 1 1
  let table'' = setColumn table' table' h w 1 1
  calc table'' abcds
