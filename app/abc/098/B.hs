import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readString = map BS.unpack . BS.words

getInt = readInt <$> BS.getLine
getString = readString <$> BS.getLine

check :: String -> String -> String
check [] as = as
check (s : ss) as = check ss as'
  where
    as'
      | s `elem` as = as
      | otherwise = s : as

calc :: String -> String -> Int -> Int
calc _ [] n = n
calc xs (y : ys) n
  | n' > n = calc (y : xs) ys n'
  | otherwise = calc (y : xs) ys n
  where
    n' = length (check (intersect xs (y : ys)) [])

main = do
  n <- getInt
  [s] <- getString
  print $ calc "" s 0
