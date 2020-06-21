import Control.Monad
import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
readString = map BS.unpack . BS.words

getIntList = readIntList <$> BS.getLine
getNString n = map readString <$> replicateM n BS.getLine

checkNeibor :: Array (Int, Int) Char -> [(Int, Int)] -> Bool
checkNeibor sss [] = False
checkNeibor sss (hw : hws)
  | sss ! hw == '#' = True
  | otherwise = checkNeibor sss hws

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
    then do
      let hws = [(h'' - 1, w''), (h'' + 1, w''), (h'', w'' -1), (h'', w'' + 1)]
      if checkNeibor sss hws == True
        then do
          if i' /= h * w
            then calc sss h w (i + 1)
            else return True
        else return False
    else do
      if i' /= h * w
        then calc sss h w (i + 1)
        else return True

main = do
  [h, w] <- getIntList
  sss <- getNString h
  let dots = replicate (w + 2) "."
  let sss' = concat (dots ++ (map (\x -> "." ++ x ++ ".") (map head sss)) ++ dots)
  let sss'' = listArray ((0, 0), (h + 1, w + 1)) sss'
  result <- calc sss'' h w 0
  let tf = if result == True then "Yes" else "No"
  putStrLn tf
