import Control.Monad
import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
readString = map BS.unpack . BS.words

getIntList = readIntList <$> BS.getLine
getString = readString <$> BS.getLine
getNString n = map readString <$> replicateM n BS.getLine

getNeibor :: Int -> Int -> Int -> Int -> [[Int]]
getNeibor w' h' w h = do
  let hs = [h' - 1, h', h' + 1]
  let ws = [w' - 1, w', w' + 1]
  filter (\x -> x /= [h', w']) (sequence [hs, ws])

sumBomb :: Array (Int, Int) Char -> [[Int]] -> Int
sumBomb sss [] = 0
sumBomb sss (ns : nss)
  | rc == '#' = 1 + sumBomb sss nss
  | otherwise = sumBomb sss nss
  where
    rc = sss ! (head (ns), last (ns))

calc sss h w i = do
  let i' = i + 1
  let h' = i' `div` w
  let w' = i' `mod` w
  let w''
        | w' == 0 = w
        | otherwise = w'
  let h''
        | w' == 0 = h'
        | otherwise = h' + 1
  if sss ! (h'', w'') == '#'
    then putStr "#"
    else do
      let nss = getNeibor w'' h'' w h
      putStr $ show (sumBomb sss nss)
  if w'' == w
    then do
      putStr "\n"
      when (i' /= h * w) $ calc sss h w (i + 1)
    else calc sss h w (i + 1)

main = do
  [h, w] <- getIntList
  sss <- getNString h
  let dots = replicate (w + 2) "."
  let sss' = concat (dots ++ (map (\x -> "." ++ x ++ ".") (map head sss)) ++ dots)
  let sss'' = listArray ((0, 0), (h + 1, w + 1)) sss'
  calc sss'' h w 0
