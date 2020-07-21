import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger

readString = map BS.unpack . BS.words

getInt = readInt <$> BS.getLine

getNString n = map readString <$> replicateM n BS.getLine

comb :: Int -> [a] -> [[a]]
comb 0 xs = [[]]
comb _ [] = []
comb n (x : xs) = [x : y | y <- comb (n -1) xs] ++ comb n xs

counts :: [String] -> Integer -> Integer -> Integer -> Integer -> Integer -> [Integer]
counts [] a b c d e = [a, b, c, d, e]
counts (s : ss) a b c d e
  | s' == 'M' = counts ss (a + 1) b c d e
  | s' == 'A' = counts ss a (b + 1) c d e
  | s' == 'R' = counts ss a b (c + 1) d e
  | s' == 'C' = counts ss a b c (d + 1) e
  | s' == 'H' = counts ss a b c d (e + 1)
  | otherwise = counts ss a b c d e
  where
    s' = head s

main = do
  n <- getInt
  ss <- getNString n
  let ss' = map head ss
  let ns = counts ss' 0 0 0 0 0
  let ns' = filter (> 0) ns
  let r
        | l < 3 = 0
        | l == 3 = product ns'
        | l > 3 = sum (map product (comb 3 ns'))
        where
          l = length ns'
  print r
