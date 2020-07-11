-- TLE / RE
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Maybe
import Data.Array
import Data.List

readInt = fst . fromJust . BS.readInt

readString = map BS.unpack . BS.words

getInt = readInt <$> BS.getLine

getString = readString <$> BS.getLine

dec2bin x
  | x <= 1 = (show (x `mod` 2))
  | otherwise = (dec2bin (x `div` 2)) ++ (show (x `mod` 2))

bin2dec lis = truncate (inner_bin2dec (reverse lis) 0)

inner_bin2dec [x] n = (read [x]) * 2 ** n
inner_bin2dec (x : rest) n =
  (read [x]) * 2 ** n
    + (inner_bin2dec rest (n + 1))

popcount b = length (filter (\c -> c == '1') b)

solve' :: String -> Int -> Int
solve' b i = do
  let i' = i + 1
  let d = bin2dec b
  let p = popcount b
  let r = d `mod` p
  if r /= 0
    then do
      let b' = dec2bin r
      solve' b' i'
    else i'

solve bit x = do
  let c = x ! bit
  let c' = if c == '0' then '1' else '0'
  let x' = x // [(bit, c')]
  let x'' = elems x'
  solve' x'' 0

main = do
  n <- getInt
  [x] <- getString
  let x' = listArray (1,n) x
  let bits = [1 .. n]
  let x'' = map (\bit -> solve bit x') bits
  mapM_ print x''
