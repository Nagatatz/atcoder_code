import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine

getIntList = readIntList <$> BS.getLine

comb :: Int -> [a] -> [[a]]
comb 0 xs       = [[]]
comb _ []       = []
comb n (x : xs) = [x : y | y <- comb (n -1) xs] ++ comb n xs

main = do
  n <- getInt
  as <- getIntList
  putStrLn $ if any (\xs -> sum xs == 1000) (comb 3 as) then "Yes" else "No"
