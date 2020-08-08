import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

getIntListN n = map readInt <$> replicateM (fromIntegral n) BS.getLine

lcms :: [Int] -> Int -> Int
lcms [] n = n
lcms (x : xs) n = lcms xs (lcm x n)

main = do
  n <- getInt
  ts <- getIntListN n
  print $ lcms ts 1
