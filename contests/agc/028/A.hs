import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

readString = map BS.unpack . BS.words

getIntList = readIntList <$> BS.getLine

getString = readString <$> BS.getLine

trim [] _ _ = ""
trim (c : cs) x x'
  | x' == 0 = c : trim cs x (x -1)
  | otherwise = trim cs x (x' -1)

main = do
  [n, m] <- getIntList
  [s] <- getString
  [t] <- getString
  let g = gcd n m
  let n' = n `div` g
  let m' = m `div` g
  print $ if trim s n' 0 == trim t m' 0 then g * n' * m' else (-1)
