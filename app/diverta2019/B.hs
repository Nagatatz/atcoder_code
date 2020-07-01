import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

calc :: Int -> Int -> Int -> Int -> Int
calc g b n (-1) = 0
calc g b n j
  | (n - j * g) `mod` b == 0 = 1 + calc g b n (j -1)
  | otherwise = calc g b n (j -1)

calcAll :: Int -> Int -> Int -> Int -> Int -> Int
calcAll r g b n (-1) = 0
calcAll r g b n i = calc g b rest j + calcAll r g b n (i -1)
  where
    rest = n - r * i
    j = rest `div` g

main = do
  [r, g, b, n] <- getIntList
  let [r', g', b'] = reverse $ sort [r, g, b]
  print $ calcAll r' g' b' n (n `div` r')
