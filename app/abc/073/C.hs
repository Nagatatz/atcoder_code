import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntListN n = map readInt <$> replicateM (fromIntegral n) BS.getLine

simulate :: [Int] -> Int -> Int -> Int
simulate [] c n
  | odd n = 1
  | otherwise = 0
simulate (a : as) c n
  | c == 0 = simulate as a 1
  | c == a = simulate as c (n + 1)
  | odd n = 1 + simulate as a 1
  | otherwise = simulate as a 1

main = do
  n <- getInt
  as <- getIntListN n
  let as' = sort as
  print $ simulate as' 0 0
