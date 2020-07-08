import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine

getIntList = readIntList <$> BS.getLine

simulate [] ds = "YES"
simulate ts [] = "NO"
simulate (t : ts) (d : ds)
  | t == d = simulate ts ds
  | t < d = "NO"
  | t > d = simulate (t : ts) ds

main = do
  n <- getInt
  ds <- getIntList
  m <- getInt
  ts <- getIntList
  let ds' = sort ds
  let ts' = sort ts
  putStrLn $ simulate ts' ds'
