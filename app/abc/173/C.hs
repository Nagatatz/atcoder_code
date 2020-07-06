import Control.Monad
import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

readString = map BS.unpack . BS.words

getIntList = readIntList <$> BS.getLine

getNString n = map readString <$> replicateM n BS.getLine

powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs

paintH2 :: Int -> Int -> Int -> Array (Int, Int) Char -> Array (Int, Int) Char
paintH2 h i w sss
  | i == w = sss'
  | otherwise = paintH2 h (i + 1) w sss'
  where
    sss' = sss // [((h, i), 'v')]

paintH :: [Int] -> Int -> Array (Int, Int) Char -> Array (Int, Int) Char
paintH [] w sss = sss
paintH (h : hs) w sss = paintH hs w sss'
  where
    sss' = paintH2 h 1 w sss

paintW2 :: Int -> Int -> Int -> Array (Int, Int) Char -> Array (Int, Int) Char
paintW2 w i h sss
  | i == h = sss'
  | otherwise = paintW2 w (i + 1) h sss'
  where
    sss' = sss // [((i, w), 'v')]

paintW :: Int -> [Int] -> Array (Int, Int) Char -> Array (Int, Int) Char
paintW h [] sss = sss
paintW h (w : ws) sss = paintW h ws sss'
  where
    sss' = paintW2 w 1 h sss

simulate2 :: [Int] -> [Int] -> Int -> Int -> Int -> Array (Int, Int) Char -> Int
simulate2 hs ws k h w sss = if tf then 1 else 0
  where
    sss' = paintH hs w sss
    sss'' = paintW h ws sss'
    tf = length (filter (\x -> x == '#') (elems sss'')) == k

simulate1 :: [[Int]] -> [Int] -> Int -> Int -> Int -> Array (Int, Int) Char -> Int -> Int
simulate1 [] ws k h w sss n = n
simulate1 (hs : hss) ws k h w sss n = simulate1 hss ws k h w sss (n + n')
  where
    n' = simulate2 hs ws k h w sss

simulate :: [[Int]] -> [[Int]] -> Int -> Int -> Int -> Array (Int, Int) Char -> Int -> Int
simulate hss [] k h w sss n = n
simulate hss (ws : wss) k h w sss n = simulate hss wss k h w sss (n + n')
  where
    n' = simulate1 hss ws k h w sss 0

main = do
  [h, w, k] <- getIntList
  sss <- getNString h
  let sss' = concat (map head sss)
  let sss'' = listArray ((1, 1), (h, w)) sss'
  let hss = powerset [1 .. h]
  let wss = powerset [1 .. w]
  let n = simulate hss wss k h w sss'' 0
  print n
