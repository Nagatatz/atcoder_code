import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
import Data.Ord

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

elemCountOrd :: (Ord a) => [a] -> [(a, Int)]
elemCountOrd = map (\l -> (head l, length l)) . group . sort

main = do
  [n, k] <- getIntList
  as <- getIntList
  let as' = elemCountOrd as
  let las = length as'
  if las <= k
    then print 0
    else print $ sum (map (\x -> snd x) (take (las - k) (sortBy (comparing snd) as')))
