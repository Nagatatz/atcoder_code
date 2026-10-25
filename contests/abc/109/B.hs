import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readString = map BS.unpack . BS.words

getInt = readInt <$> BS.getLine
getNString n = map readString <$> replicateM n BS.getLine

check :: [String] -> [String] -> Char -> String
check _ [] c = "Yes"
check ds (w : ws) c
  | w `elem` ds = "No"
  | c == head w = check (w : ds) ws (last w)
  | otherwise = "No"

main = do
  n <- getInt
  ws <- getNString n
  let ws' = map head ws
  putStrLn $ check [head ws'] (tail ws') (last (head ws'))
