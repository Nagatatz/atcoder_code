import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

check :: [Int] -> [Int] -> [Int]
check [] as = as
check (s : ss) as = check ss as'
  where
    as'
      | s `elem` as = as
      | otherwise = s : as

main = do
  n <- getInt
  as <- getIntList
  let as' = map (\x -> x `div` 400) as
  let overNum = length (filter (\x -> x >= 8) as')
  let defaultNum = length (check (filter (\x -> x < 8) as') [])
  let minNum = maximum[1, defaultNum]
  let maxNum = defaultNum + overNum
  putStrLn $ show minNum ++ " " ++ show maxNum
