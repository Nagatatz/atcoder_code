import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Maybe

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

split :: String -> Int -> Int -> (Int, Int)
split [] o e = (o, e)
split [x] o e = split [] o' e
  where
    o' = if x == '1' then o + 1 else o
split (x : y : ss) o e = split ss o' e'
  where
    o' = if x == '1' then o + 1 else o
    e' = if y == '1' then e + 1 else e

main = do
  [s] <- getString
  let k = if odd (length s) then 1 else 0
  let hl = length s `div` 2
  let (ol, el) = split s 0 0
  if ol > el
    then print $ hl + k - ol + el
    else print $ ol + hl - el
