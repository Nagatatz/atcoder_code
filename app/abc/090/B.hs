import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

getKaiNum :: [[Int]] -> Int -> Int -> Int
getKaiNum [] a b = 0
getKaiNum (x : xs) a b = m + getKaiNum xs a b
  where
    [s, t] = map . show x
    n = read (s ++ t ++ reverse s) :: Int
    m = if n >= a && n <= b then 1 else 0

main = do
  [a, b] <- getIntList
  let xs = sequence [[10 .. 99], [0 .. 9]]
  print $ getKaiNum xs a b
