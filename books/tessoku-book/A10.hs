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

scanMax :: Ord a => [a] -> [a]
scanMax = scanl1 max

printResult ([l, r] : lrs) len as' as'' = do
  print $ max (as' ! (l - 1)) (as'' ! (len - r))
  when (lrs /= []) $ printResult lrs len as' as''

main = do
  n <- getInt
  as <- getIntList
  d <- getInt
  lrs <- getIntNList d
  let len = length as
  let as' = listArray (1, len) (scanMax as)
  let as'' = listArray (1, len) (scanMax (reverse as))
  printResult lrs len as' as''
