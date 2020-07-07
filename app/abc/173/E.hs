import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
import Data.Ord

readInt = fst . fromJust . BS.readInt

readInteger = fst . fromJust . BS.readInteger

readIntList = map readInt . BS.words

readIntegerList = map readInteger . BS.words

getIntList = readIntList <$> BS.getLine

getIntegerList = readIntegerList <$> BS.getLine

mod1G7 n = n `mod` (10 ^ 9 + 7)

main :: IO ()
main = do
  [n, k] <- getIntList
  as <- getIntegerList
  if all (<= 0) as
    then
      if odd k
        then print $ mod1G7 (product(take k (sortBy (comparing (\a -> (abs a))) as)))
        else print $ mod1G7 (product(take k (sortBy (comparing (\a -> (- (abs a)))) as)))
    else do
      let as' = sortBy (comparing (\a -> (- (abs a)))) as
      let bs = take k as'
      let s = product bs
      if s > 0
        then print $ mod1G7 (s)
        else do
          let bs' = filter (\b' -> b' < 0) (reverse bs)
          let as'' = filter (\a -> a > 0) (drop k as')
          let c = if length as'' == 0 || length bs' == 0 then 0 else (product (bs) `div` (head (bs'))) * (head as'')
          let bs'' = filter (\b' -> b' > 0) (reverse bs)
          let as''' = filter (\a -> a < 0) (drop k as')
          let c' = if length as''' == 0 || length bs'' == 0 then 0 else (product (bs) `div` (head (bs''))) * (head as''')
          if c == 0 && c' == 0
            then print $ mod1G7 (product (take k (filter (\a -> a <= 0) (reverse as'))))
            else print $ mod1G7 (maximum [c, c'])
