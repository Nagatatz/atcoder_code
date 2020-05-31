import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine
getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

createAs :: [[Int]] -> [Int]
createAs = map head

createBs :: [[Int]] -> [Int]
createBs = map last

main = do
  n <- getInt
  xss <- getIntNList n
  let len = length xss
  let cent = len `div` 2
  let isOdd = odd len
  let as = sort (createAs xss)
  let bs = sort (createBs xss)
  if isOdd
    then print $ bs !! cent - as !! cent + 1
    else print (bs !! cent + bs !! (cent - 1) - (as !! cent + as !! (cent - 1)) + 1)
