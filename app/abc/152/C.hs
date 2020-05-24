import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

judge :: Array Int Int -> Int -> Int -> Int -> Int
judge xs n' n mi
  | n' > n = 0
  | x <= mi = 1 + judge xs (n' + 1) n x
  | otherwise = judge xs (n' + 1) n mi
  where
    x = xs ! n'

main = do
  n <- getInt
  ps <- getIntList
  let ps' = listArray (1, length ps) ps
  print $ judge ps' 1 n (ps' ! 1)
