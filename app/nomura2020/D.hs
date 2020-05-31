-- NEVER END
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

createArray :: [Int] -> [[Int, Int]]
createArray [] _ = []
createArray (x:xs) n = [n, x] ++ createArray xs (n+1)

main = do
  n <- getInt
  ps <- getIntList
  if ps `elem` (-1)
    then do
      -- set
      let notMi = filter (/= -1) ps
      -- -1 参照数
      let mi = length (filter (== -1) ps)
      -- (n-1)^k * (set+(-1数)) - 参照数
      
    else print 1