-- TLE

import Data.Array
import Data.Bits
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

getVU n = VU.reverse . VU.unfoldrN n BS.uncons <$> BS.getLine

simulate :: Integer -> Integer -> Integer
simulate count 0 = count
simulate count x = simulate (count + 1) (x `mod` (fromIntegral (popCount x)))

solve ::  Integer -> Int -> Integer -> Integer -> Integer -> Integer -> Int -> Integer
solve x n p0 p1 s0 s1 b = do
  let x' = complementBit x b
  if x' == 0
    then 0
    else do
      let b' = testBit x' (fromIntegral b)
      let d = if b' then (2 ^ (fromIntegral b) `mod` p0 + s0) `mod` p0 else (- 2 ^ (fromIntegral b) `mod` p1 + s1) `mod` p1
      simulate 1 d

createBits :: VU.Vector Char -> Integer
createBits = VU.ifoldl' (\bit index value -> if value == '1' then setBit bit index else bit) zeroBits

main = do
  n <- getInt
  x <- getVU n
  let x' = createBits x
  let ppc = fromIntegral (popCount x')
  let p0 = ppc + 1
  let p1 = ppc - 1
  let s0 = x' `mod` p0
  let s1 = x' `mod` p1
  let bs = reverse [0 .. (fromIntegral n -1)]
  mapM_ (print . (solve x' n p0 p1 s0 s1)) bs
