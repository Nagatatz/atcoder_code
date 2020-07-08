import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine

getIntList = readIntList <$> BS.getLine

sep :: [Int] -> [Int] -> [[Int]]
sep [] ns = [ns]
sep (h : hs) ns
  | h == 0 = ns : sep hs []
  | otherwise = sep hs (h : ns)

calc :: [Int] -> Int
calc [] = 0
calc ls = lmin + sum (map (\ys -> calc ys) separated)
  where
    lmin = minimum ls
    separated = filter (\as -> as /= []) (sep (map (\x -> x - lmin) ls) [])

main = do
  n <- getInt
  hs <- getIntList
  print $ calc hs
