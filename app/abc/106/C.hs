import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readString = map BS.unpack . BS.words

getInt = readInt <$> BS.getLine
getString = readString <$> BS.getLine

simulate (s : ss) k
  | k == 1 = [s]
  | s == '1' = simulate ss (k -1)
  | otherwise = [s]

main = do
  [ss] <- getString
  k <- getInt
  putStrLn $ simulate ss k
