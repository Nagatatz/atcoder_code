import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

calc :: Int -> Int -> Int
calc 1 x = x
calc n x = calc (n - 1) ((n * x) `mod` (10 ^ 9 + 7))

mul' :: Int -> Int -> Int
mul' a b = a * b `mod` (10 ^ 9 + 7)

main = do
  [n, m] <- getIntList
  let diff = abs (n - m)
  if diff > 1
    then print 0
    else
      if diff == 0
        then print $ mul' (mul' (calc n 1) (calc m 1)) 2
        else print $ mul' (calc n 1) (calc m 1)
