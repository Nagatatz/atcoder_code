import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

calc :: String -> [Int]
calc s
  | length s >= 3 = abs ((read (take 3 s) :: Int) - 753) : calc (tail s)
  | otherwise = []

main = do
  [s] <- getString
  print $ minimum (calc s)
