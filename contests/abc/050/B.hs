import Control.Monad
import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine
getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

calc :: Int -> Array Int Int -> Int -> Int -> Int
calc tSum ts' p x = tSum - ts' ! p + x

calcAll tSum ts' (ps : pss) = do
  print $ calc tSum ts' (head ps) (last ps)
  when (pss /= []) $ calcAll tSum ts' pss

main = do
  n <- getInt
  ts <- getIntList
  m <- getInt
  pss <- getIntNList m
  let tSum = sum ts
  let ts' = listArray (1, length ts) ts
  calcAll tSum ts' pss
