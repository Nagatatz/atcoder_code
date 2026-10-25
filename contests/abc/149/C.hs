import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger

getInt = readInt <$> BS.getLine

createDivision :: Int -> [Int]
createDivision n = 2 : filter (\ x -> x `mod` 2 /= 0) [2 .. divMax]
  where
    divMax = n `div` 2

isPrime :: Int -> [Int] -> Bool
isPrime x [] = True
isPrime x (n : ns)
  | x `mod` n == 0 = False
  | otherwise = isPrime x ns

leastPrime :: Int -> Int
leastPrime x
  | isPrime x (createDivision x) = x
  | otherwise = leastPrime (x + 1)

main = do
  x <- getInt
  if x == 2 then
    print x
  else do
    print $ leastPrime x