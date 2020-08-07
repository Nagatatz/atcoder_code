import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine

getIntList = readIntList <$> BS.getLine

simulate :: [Int] -> Int -> Int -> Int
simulate _ 0 _ = 0
simulate [x] ans _ = ans
simulate (x : xs) (-1) (-1) = simulate xs ans diff
  where
    diff = x - sum xs
    ans = abs diff
simulate (x : xs) ans diff = simulate xs ans'' diff'
  where
    diff' = diff + 2 * x
    ans' = abs diff
    ans'' = if ans > ans' then ans' else ans

main = do
  n <- getInt
  as <- getIntList
  print $ simulate as (-1) (-1)
