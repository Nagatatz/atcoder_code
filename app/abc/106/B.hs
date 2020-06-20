import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

calc :: Int -> Int -> Int -> Int
calc a n x
  | a > n `div` 2 = x
  | n `mod` a == 0 = calc (a + 1) n (x + 1)
  | otherwise = calc (a + 1) n x

calcAll :: Int -> Int -> Int -> Int
calcAll a n x
  | a > n = x
  | a `mod` 2 == 0 = calcAll (a + 1) n x
  | (calc 1 a 0) == 7 = calcAll (a + 1) n (x + 1)
  | otherwise = calcAll (a + 1) n x

main = do
  n <- getInt
  print $ calcAll 1 n 0
