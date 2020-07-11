-- WA
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

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

solve :: Int -> VUM.IOVector Char -> IO (Int)
solve bit x = do
  c <- VUM.read x bit
  let c' = if c == '0' then '1' else '0'
  VUM.write x bit c'
  x' <- VU.freeze x
  let x'' = VU.toList x'
  let i = solve' x'' 0
  return i

main = do
  n <- getInt
  [x] <- getString
  let x' = VU.fromList x
  x'' <- VU.thaw x'
  let bits = [0 .. n - 1]
  xs <- mapM (\bit -> solve bit x'') bits
  mapM_ print (xs)
