import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.List

readInt = fst . fromJust . BS.readInt
readString = map BS.unpack . BS.words

getInt = readInt <$> BS.getLine
getNString n = map readString <$> replicateM n BS.getLine

elemCountOrd :: (Ord a) => [a] -> [(a, Int)]
elemCountOrd = map (\l -> (head l, length l) ) . group . sort

main = do
  n <- getInt
  ss <- getNString n
  let ss' = sort (map head ss)
  let ss'' = elemCountOrd ss'
  let maxCount = maximum (map (\x -> snd x) ss'')
  let elems = map (\x -> fst x) (filter (\x -> (snd x) == maxCount) ss'')
  mapM_ putStrLn elems
