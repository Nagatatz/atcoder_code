import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

readString = map BS.unpack . BS.words

getIntList = readIntList <$> BS.getLine

getString = readString <$> BS.getLine

checkRock :: Array Int Char -> Int -> Int -> Char -> Bool
checkRock ss i n c
  | i > n = True
  | c == '#' && c' == '#' = False
  | otherwise = checkRock ss (i + 1) n c'
  where
    c' = ss ! i

checkTriplet :: Array Int Char -> Int -> Int -> Char -> Char -> Bool
checkTriplet ss i n c1 c2
  | i > n = False
  | c2 == '#' && c' == '#' = False
  | c1 == '.' && c2 == '.' && c' == '.' = True
  | otherwise = checkTriplet ss (i + 1) n c2 c'
  where
    c' = ss ! i

main = do
  [n, a, b, c, d] <- getIntList
  [ss] <- getString
  let ss' = listArray (1, n) ss
  if b > c
    then if checkRock ss' a b '.' && checkRock ss' c d '.' then putStrLn "Yes" else putStrLn "No"
    else do
      if d > c
        then if checkRock ss' a d '.' == False then putStrLn "No" else do putStrLn "Yes"
        else if checkRock ss' a c '.' && checkTriplet ss' (b + 1) (d + 1) (ss' ! (b -1)) (ss' ! b) then putStrLn "Yes" else do putStrLn "No"
