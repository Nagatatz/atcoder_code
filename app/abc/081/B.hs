import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

divide :: [Int] -> Int
divide as
  | all (\x -> x `mod` 2 == 0) as = 1 + divide (map ((`div` 2)) as)
  | otherwise = 0

main = do
  n <- getInt
  as <- getIntList
  print $ divide as
