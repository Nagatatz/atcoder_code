import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.List

readInteger = fst . fromJust . BS.readInteger
readIntegerList = map readInteger . BS.words

getInteger = readInteger <$> BS.getLine
getIntegerNList n = map readIntegerList <$> replicateM (fromIntegral n) BS.getLine

average :: (Real a) => [a] -> Double
average xs = (realToFrac $ sum xs) / fromIntegral (length xs)

calc :: Integer -> [Integer] -> Integer
calc xc [] = 0
calc xc (x : xs) = abs (x - xc) + calc xc xs

calcAll :: [Integer] -> [Integer] -> Integer -> Integer
calcAll [] xs sumx = sumx
calcAll (xc : xcs) xs sumx
  | sumx == (-1) = calcAll xcs xs calc'
  | calc' < sumx = calcAll xcs xs calc'
  | otherwise = calcAll xcs xs sumx
  where
    calc' = calc xc xs

main = do
  n <- getInteger
  xys <- getIntegerNList n
  let base = sum (map (\xs -> last xs - head xs) xys)
  let xs = sort (map head xys)
  let lenx = (length xs) `div`  2
  let xcs = [xs !! (lenx), xs !! (lenx + 1)]
  let ys = sort (map last xys)
  let ycs = [ys !! (lenx), ys !! (lenx + 1)]
  print $ calcAll xcs xs (-1) + calcAll ycs ys (-1) + base
