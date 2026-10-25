import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger

readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine

getIntList = readIntList <$> BS.getLine

getNum :: [Int] -> Int
getNum xs
  | l > h = l - h
  | l == h = 0
  | l < h = l
  where
    h = head xs
    l = length xs

elemCountOrd :: [Int] -> Int
elemCountOrd = sum . map (\xs -> getNum xs) . group . sort

main = do
  n <- getInt
  as <- getIntList
  print $ elemCountOrd as
