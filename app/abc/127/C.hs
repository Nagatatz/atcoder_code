import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine
getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

limit :: [[Int]] -> Int -> Int -> Int
limit [] mi ma = ma - mi + 1
limit (lr : lrs) mi ma
  | mi <= ma = limit lrs mi' ma'
  | otherwise = 0
  where
    l = head lr
    r = last lr
    mi' = maximum [l, mi]
    ma' = minimum [r, ma]

main = do
  [n, m] <- getIntList
  lrs <- getIntNList m
  print $ limit lrs 1 n
