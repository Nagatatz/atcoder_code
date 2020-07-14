import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine

getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

calc :: [[Int]] -> Int -> String
calc [] _ = "Yes"
calc (ab : abs) n
  | n' <= b = calc abs n'
  | otherwise = "No"
  where
    [a, b] = ab
    n' = n + a

main = do
  n <- getInt
  abs <- getIntNList n
  let abs' = sortBy (compare `on` last) abs
  putStrLn $ calc abs' 0
