import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger

readString = map BS.unpack . BS.words

getInt = readInt <$> BS.getLine

getString = readString <$> BS.getLine

productP :: [Int] -> Int -> Int
productP [] p = p
productP (x : xs) p = productP xs ((x + 1) * p `mod` (10 ^ 9 + 7))

sum' :: [Int] -> Int -> Int
sum' [] s = s
sum' (x : xs) s = sum' xs ((x + s) `mod` (10 ^ 9 + 7))

main = do
  n <- getInt
  [s] <- getString
  let s' = map length (group (sort s))
  print $ sum' [(productP s' 1), (-1)] 0
