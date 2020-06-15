import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

calc :: [Int] -> Int -> Int -> Int -> Int
calc [] n a d = a
calc (p : ps) n a d = calc ps n a' d'
  where
    ds = abs (n - p)
    d'
      | ds < d = ds
      | otherwise = d
    a'
      | ds < d = p
      | otherwise = a

main = do
  [x, n] <- getIntList
  ps <- getIntList
  if n == 0
    then print x
    else do
      let ps_max = maximum ps
      let ns = filter (\x -> x `notElem` ps) [1..ps_max+1]
      print $ calc ns x 0 x
