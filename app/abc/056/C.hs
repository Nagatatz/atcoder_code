import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

calc :: [Int] -> Int -> Int -> Int -> Int
calc (n : ns) c i x
  | (c + n) >= x = i
  | otherwise = calc ns (c + n) (i + 1) x

main = do
  x <- getInt
  print $ calc [1 ..] 0 1 x
