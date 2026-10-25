import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

checkDivisorOfHundred :: Int -> Int -> Bool
checkDivisorOfHundred x b
  | x > 100 = False
  | x > b = False
  | x == 100 = True
  | otherwise = (100 `mod` x == 0) || checkDivisorOfHundred (x + 1) b

main = do
  [a, b] <- getIntList
  putStrLn $ if checkDivisorOfHundred a b then "Yes" else "No"
