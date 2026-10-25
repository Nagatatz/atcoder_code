import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine
getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

calc table (x : xs) = do
  count_x <- VUM.read table (x -1)
  VUM.write table (x -1) (count_x + 1)
  when (xs /= []) $ calc table xs

printCount table n n' = do
  count_n <- VUM.read table n'
  print count_n
  when (n /= n') $ printCount table n (n' + 1)

main = do
  [n, m] <- getIntList
  xss <- getIntNList m
  let xss' = concat xss
  table <- VUM.replicate n 0 :: IO (VUM.IOVector Int)
  calc table xss'
  printCount table (n -1) 0
