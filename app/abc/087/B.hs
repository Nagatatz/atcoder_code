import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

divide :: Int -> Int -> Int -> Int -> Int
divide (-1) b c x = 0
divide a b c x = count + divide (a -1) b c x
  where
    remain = x - 500 * a
    count =
      if remain <= 100 * b + 50 * c
        then divideBc b c remain
        else 0

divideBc :: Int -> Int -> Int -> Int
divideBc (-1) c x = 0
divideBc b c x = count + divideBc (b -1) c x
  where
    remain = x - 100 * b
    count =
      if remain >= 0
        then do
          let divided = remain `div` 50
          if divided <= c
            then 1
            else 0
        else 0

main = do
  a <- getInt
  b <- getInt
  c <- getInt
  x <- getInt
  print $ divide a b c x
