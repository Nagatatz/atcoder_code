import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

getIntListN n = map readInt <$> replicateM (fromIntegral n) BS.getLine

pick :: [Int] -> Int -> Int -> Int -> Int
pick [] t' k i = 0
pick (t : ts) t' k i
  | i == 0 = 0
  | t > t' + k = 0
  | t <= t' + k = 1 + pick ts t' k (i - 1)

calc :: [Int] -> Int -> Int -> Int
calc [] c k = 0
calc ts c k = 1 + calc ts' c k
  where
    t = head ts
    n = pick ts t k c
    ts' = drop n ts

main = do
  [n, c, k] <- getIntList
  ts <- getIntListN n
  let ts' = sort ts
  print $ calc ts' c k
