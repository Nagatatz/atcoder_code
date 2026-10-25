import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

simulate i k n' n
  | i > k = [-1, -1]
  | m == 0 && b' >= 0 && b' <= n && (i + b') <= n = [i, b']
  | otherwise = simulate (i + 1) k n' n
  where
    n'' = n' - 9 * i
    b' = n'' `div` 4
    m = n'' `mod` 4

main = do
  [n, y] <- getIntList
  let c = y `div` 1000
  if n > c
    then putStrLn "-1 -1 -1"
    else do
      let n' = c - n
      let [a, b] = simulate 0 (n' `div` 9) n' n
      if a == (-1)
        then putStrLn "-1 -1 -1"
        else do
          let c' = n - (a + b)
          putStrLn $ (show a) ++ " " ++ (show b) ++ " " ++ (show c')
