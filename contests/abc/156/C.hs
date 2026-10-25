import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

calc :: [Int] -> Int -> Int
calc xs p = foldr (\ x -> (+) ((x - p) ^ 2)) 0 xs

main = do
  n <- getInt
  xs <- getIntList
  -- sum(2p - 2x_i) が 0 になるとき極小
  let x_sum = sum xs
  let d = x_sum `div` n
  let m = x_sum `mod` n
  if m == 0 then
    print $ calc xs m
  else do
    let a = calc xs m
    let b = calc xs (m + 1)
    if a < b then
      print a
    else
      print b