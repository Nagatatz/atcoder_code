import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

calc :: [Int] -> [[Int]] -> Int -> Int -> Float
calc xy [] i j = 0
calc xy (xy' : xys') i j
  | i == j = 0
  | otherwise = sqrt (fromIntegral((head xy - head xy') ^ 2 + (last xy - last xy') ^ 2)) + (calc xy xys' i (j + 1))

calcAll :: [[Int]] -> [[Int]] -> Int -> Float
calcAll [] xys' i = 0
calcAll (xy : xys) xys' i = calc xy xys' i 0 + calcAll xys xys' (i + 1)

main = do
  n <- getInt
  xys <- getIntNList n
  print $ (calcAll xys xys 0) / (fromIntegral n) * 2.0
