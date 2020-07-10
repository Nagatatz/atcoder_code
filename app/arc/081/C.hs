import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine

getIntList = readIntList <$> BS.getLine

createTuple :: [[Int]] -> [(Int, Int)]
createTuple [] = []
createTuple (xs : xss)
  | length xs >= 2 = (head xs, length xs) : createTuple xss
  | otherwise = createTuple xss

main = do
  n <- getInt
  as <- getIntList
  let as' = createTuple (group (reverse(sort as)))
  if length as' == 0
    then print 0
    else do
      let c = snd (head as')
      let n
            | c >= 4 = (fst (head as')) ^ 2
            | c >= 2 && snd (head (tail as')) >= 2 = fst (head as') * fst (head (tail as'))
            | otherwise = 0
      print n
