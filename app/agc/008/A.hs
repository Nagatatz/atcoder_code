import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

calc x y
  | x > 0 && y > 0 && x' < y' = s
  | x > 0 && y > 0 && x' > y' = s + 2
  | x > 0 && y < 0 && x' < y' = s + 1
  | x > 0 && y < 0 && x' >= y' = s + 1
  | x < 0 && y > 0 && x' <= y' = s + 1
  | x < 0 && y > 0 && x' > y' = s + 1
  | x < 0 && y < 0 && x' < y' = s + 2
  | x < 0 && y < 0 && x' > y' = s
  | x == 0 && y > 0 = s
  | x == 0 && y < 0 = s + 1
  | x < 0 && y == 0 = s
  | x > 0 && y == 0 = s + 1
  where
    y' = abs y
    x' = abs x
    s = abs (y' - x')

main = do
  [x, y] <- getIntList
  print $ calc x y
