import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readString = map BS.unpack . BS.words

getInt = readInt <$> BS.getLine

getString = readString <$> BS.getLine

getNString n = map readString <$> replicateM n BS.getLine

calc :: String -> [[Char]] -> Int -> String -> String -> String
calc a [] d fs rs'
  | d == 1 = fs ++ a ++ reverse rs'
  | d == (-1) = rs' ++ reverse a ++ reverse fs
calc a (q : qs) d fs rs'
  | q' == '1' = calc a qs (- d) fs rs'
  | d == 1 && f == '1' = calc a qs d (c : fs) rs'
  | d == 1 && f == '2' = calc a qs d fs (c : rs')
  | d == (-1) && f == '1' = calc a qs d fs (c : rs')
  | d == (-1) && f == '2' = calc a qs d (c : fs) rs'
  where
    q' = head q
    f = if q' == '2' then head (tail q) else '0'
    c = if q' == '2' then head (tail (tail q)) else '_'

main = do
  [a] <- getString
  n <- getInt
  qs <- getNString n
  let qs' = map (\q -> map head q) qs
  putStrLn $ calc a qs' 1 "" ""
