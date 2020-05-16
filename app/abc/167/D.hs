--TLE

import Data.Maybe
import Data.List
import qualified Data.ByteString.Char8 as BS

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

--teleport :: Num a => [a] -> a -> a
teleport xs i = xs !! i

--teleport_list :: Num a => [a] -> a -> a -> [a]
teleport_list xs i 0 = i : []
teleport_list xs i n = i : teleport_list xs (teleport xs i - 1) (n - 1)

--get_rep :: Num a => [a] -> a -> [a]
get_rep xs 0 = [0, length(xs) - 1] 
get_rep xs n | index == Nothing = get_rep xs (n - 1)
             | otherwise = [search_index, fromJust index + search_index]
             where
               search_index = length xs - n
               remaining = drop (search_index + 1) xs
               index = elemIndex (xs !! search_index) remaining 

main = do
  [n_g, k_g] <- getIntList
  xs_g <- getIntList
  let xs = map fromIntegral xs_g
  let n = fromIntegral n_g
  let k = fromIntegral k_g
  let teleport_n = teleport_list xs 0 n
  let [loop_start, loop_end] = get_rep teleport_n (n+1)
  let modulation = loop_end - loop_start + 1
  if k < loop_start then
    putStrLn $ show (teleport_n !! (k+1))
  else do
    let teleport_count = k - loop_start
    let teleport_index = loop_start + teleport_count `mod` modulation
    putStrLn $ show (teleport_n !! teleport_index + 1)