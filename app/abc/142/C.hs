import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

sortList :: [Int] -> Int -> [Int]
sortList xs 0 = []
sortList xs n = sortList xs (n -1) ++ [fromJust (elemIndex n xs) + 1]

convertListAsString :: [Int] -> String
convertListAsString [] = ""
convertListAsString [x] = show x
convertListAsString (x : xs) = show x ++ " " ++ convertListAsString xs

main = do
  n <- getInt
  as <- getIntList
  sortList as n
  let numList = sortList as n
  putStrLn $ convertListAsString numList
