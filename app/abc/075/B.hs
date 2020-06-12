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
  let hs
        | h' == 1 = [h', h' + 1]
        | h' == h = [h' - 1, h']
        | otherwise = [h' - 1, h', h' + 1]
  let ws
        | w' == 1 = [w', w' + 1]
        | w' == w = [w' - 1, w']
        | otherwise = [w' - 1, w', w' + 1]
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

calcable :: String -> [Int]
calcable [] = []
calcable (s : ss)
  | s == '#' = 1 : calcable ss
  | otherwise = 0 : calcable ss

calcSingle :: Array Int Int -> Int -> Int -> String
calcSingle ss n' n
  | n' == (n + 1) = []
  | otherwise = v ++ calcSingle ss (n' + 1) n
  where
    v
      | ss ! n' == 1 = "#"
      | n' == 1 = show $ ss ! 2
      | n' == n = show $ ss ! (n - 1)
      | otherwise = show $ (ss ! (n' - 1)) + (ss ! (n' + 1))

putCharLn :: Char -> IO()
putCharLn c = putStrLn $ [c]

main = do
  [h, w] <- getIntList
  if h == 1
    then do
      [sss] <- getString
      if w == 1
        then if sss == "#" then putStrLn "#" else putStrLn "0"
        else do
          let sss' = calcable sss
          let sss'' = listArray (1, w) sss'
          putStrLn $ calcSingle sss'' 1 w
    else do
      if w == 1
        then do
          sss <- getNString h
          let sss' = calcable (concat (map head sss))
          let sss'' = listArray (1, h) sss'
          let result = calcSingle sss'' 1 h
          mapM_ putCharLn result
          putStr "\n"
        else do
          sss <- getNString h
          let sss' = concat (map head sss)
          let sss'' = listArray ((1, 1), (h, w)) sss'
          calc sss'' h w 0
