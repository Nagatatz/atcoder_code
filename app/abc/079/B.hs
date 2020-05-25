import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

lucas :: [Int] -> Int -> [Int]
lucas xs (-1) = xs
lucas [] n = lucas [2] (n -1)
lucas [x] n = lucas [1, 2] (n -1)
lucas xs n = lucas ((head xs + head (tail xs)) : xs) (n -1)

main = do
  n <- getInt
  print $ head (lucas [] n)
