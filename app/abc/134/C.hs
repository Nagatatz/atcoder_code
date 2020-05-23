import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
import Data.Ord

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine
getIntListN n = map readInt <$> replicateM (fromIntegral n) BS.getLine

printing (x : xs) ma ma2 = do
  if x /= ma
    then print ma
    else print ma2
  when (xs /= []) $ printing xs ma ma2

main = do
  n <- getInt
  as <- getIntListN n
  let as' = sortOn Data.Ord.Down as
  let ma = head as'
  let ma2 = head (tail as')
  printing as ma ma2
