import qualified Data.ByteString.Char8 as BS
import Data.Function
import Data.List

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

countChar :: String -> Char -> Int -> Int -> (Char, Int)
countChar [] c a m = (c, maximum [a, m])
countChar (s : ss) c a m
  | s == c = countChar ss c 0 (maximum [a, m])
  | s /= c = countChar ss c (a + 1) m

main = do
  [s] <- getString
  let as = map (\c -> countChar s c 0 0) (map head (group (sort s)))
  print $ snd (head (sortBy (compare `on` snd) as))
