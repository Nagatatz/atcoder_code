import qualified Data.ByteString.Char8 as BS
import Data.Function
import Data.List
import Data.Maybe
import Data.Ord

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

prod = foldl1' (\x y -> mod1G7 (x * y))

mod1G7 n = n `mod` (10 ^ 9 + 7)

main :: IO ()
main = do
  [n, k] <- getIntList
  as <- getIntList
  if k == n
    then print $ prod as
    else
      if all (<= 0) as
        then
          if odd k
            then print $ prod (take k (sortBy (compare `on` Down) as))
            else print $ prod (take k (sort as))
        else do
          let as' = sortBy (comparing (\a -> (- (abs a)))) as
          let bs = take k as'
          if elem 0 bs
            then print 0
            else
              if even (length (filter (\x -> x < 0) bs))
                then print $ prod bs
                else do
                  let bsr = reverse bs
                  let asr = drop k as'
                  let bs' = filter (\b' -> b' < 0) bsr
                  let as'' = filter (\a -> a >= 0) asr
                  let c = if length as'' == 0 || length bs' == 0 then 0 else prod ((head as'') : (delete (head bs') bs))
                  let bs'' = filter (\b' -> b' > 0) bsr
                  let as''' = filter (\a -> a <= 0) asr
                  let c' = if length as''' == 0 || length bs'' == 0 then 0 else prod ((head as''') : (delete (head bs'') bs))
                  if c == 0 || c' == 0
                    then
                      if c == 0 && c' == 0
                        then print 0
                        else print $ maximum [c, c']
                    else do
                      if head as'' * head bs'' > head as''' * head bs'
                        then print c
                        else print c'
