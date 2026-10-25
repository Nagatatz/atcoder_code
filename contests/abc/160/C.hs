import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

calcDist :: [Int] -> [Int]
calcDist (x:[]) = []
calcDist (x:x2:xs) = (x2 - x) : calcDist (x2:xs)

main = do
  [k, n] <- getIntList
  xs <- getIntList
  let fl = head xs + k - last xs
  let ds = calcDist xs
  let distMax =  maximum(fl:ds)
  print k - distMax
