import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

simulate :: [Int] -> Int -> String -> Int -> Int
simulate [] a' d c = c
simulate (a : as) a' d c
  | a' == (-1) = simulate as a "even" c
  | a == a' = simulate as a' d c
  | d == "even" && a > a' = simulate as a "up" c
  | d == "even" && a < a' = simulate as a "down" c
  | d == "up" && a > a' = simulate as a "up" c
  | d == "up" && a < a' = simulate as a "even" (c + 1)
  | d == "down" && a < a' = simulate as a "down" c
  | d == "down" && a > a' = simulate as a "even" (c + 1)

main = do
  n <- getInt
  as <- getIntList
  print $ simulate as (-1) "" 0 + 1
