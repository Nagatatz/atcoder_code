import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

calc :: [Int] -> Int -> [(Int, Int)]
calc [] _ = []
calc (n : ns) ma = (d, m) : calc ns ma
  where
    n' = ma - n
    d = n' `div` 2
    m = n' `mod` 2

main = do
  ns <- getIntList
  let ts = calc ns (maximum ns)
  let s = sum (map (\t -> fst t) ts)
  let s'
        | l == 0 = s
        | l == 1 = s + 2
        | l == 2 = s + 1
        where
          l = length (filter (\t -> snd t == 1) ts)
  print s'
