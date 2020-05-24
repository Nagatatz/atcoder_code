import qualified Data.ByteString.Char8 as BS
import Control.Monad
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine
getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

getCommon :: [[Int]] -> [Int] -> [Int]
getCommon [] ys = ys
getCommon (xs:xss) [] = getCommon xss (tail xs)
getCommon (xs:xss) ys = getCommon xss (filter (\y -> y `elem` tail xs) ys)

main = do
  [n, m] <- getIntList
  ass <- getIntNList n
  print $ length (getCommon ass [])
  