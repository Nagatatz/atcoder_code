-- 参考: @matonix https://atcoder.jp/contests/aising2020/submissions/15176826
import Data.Bits
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

getVU n = VU.reverse . VU.unfoldrN n BS.uncons <$> BS.getLine

createBits :: VU.Vector Char -> Integer
createBits = VU.ifoldl' (\bit index value -> if value == '1' then setBit bit index else bit) zeroBits

simulate :: Integer -> Integer -> Integer
simulate count 0 = count
simulate count x = simulate (count + 1) (x `mod` (fromIntegral (popCount x)))

main = do
  n <- getInt
  x <- getVU n
  let x' = createBits x
  let xs = [complementBit x' i | i <- reverse [0 .. n -1]]
  mapM_ (print . (simulate 0)) xs
