import Control.Monad
import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

checkI css tup 0 _ = True
checkI css tup n v
  | n == 3 = checkI css tup (n -1) diff
  | v == diff = checkI css tup (n -1) v
  | otherwise = False
  where
    diff = css ! (fst tup, n) - css ! (snd tup, n)

checkJ css tup 0 _ = True
checkJ css tup n v
  | n == 3 = checkJ css tup (n -1) diff
  | v == diff = checkJ css tup (n -1) v
  | otherwise = False
  where
    diff = css ! (n, fst tup) - css ! (n, snd tup)

main = do
  let num = 3
  css <- getIntNList num
  let css' = listArray ((1, 1), (num, num)) (concat css)
  if checkI css' (3, 2) 3 0 && checkI css' (2, 1) 3 0 && checkJ css' (3, 2) 3 0 && checkJ css' (2, 1) 3 0
    then putStrLn "Yes"
    else putStrLn "No"
