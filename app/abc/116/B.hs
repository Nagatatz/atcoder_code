import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger

getInt = readInt <$> BS.getLine

makeAlign :: Int -> [Int]
makeAlign s
  | s `mod` 2 == 0 = s : makeAlign (s `div` 2)
  | otherwise = s : makeAlign (3 * s + 1)

judge :: [Int] -> Int -> [Int] -> Int -> Int
judge [] (-1) (z : z2 : zs) n = judge [z] z2 zs (n + 2)
judge xs y (z : zs) n
  | y `elem` xs = n
  | otherwise = judge (xs ++ [y]) z zs (n + 1)

main = do
  s <- getInt
  let align = makeAlign s
  print $ judge [] (-1) align 0
