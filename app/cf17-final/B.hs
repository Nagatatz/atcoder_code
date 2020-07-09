import qualified Data.ByteString.Char8 as BS
import Data.List

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

calc :: [[Char]] -> [(Char, Int)]
calc [] = []
calc (xs : xss) = (head xs, length xs) : calc xss

main = do
  [s] <- getString
  let s' = s ++ "abc"
  let g = calc (group (sort s'))
  let sn = map snd g
  let m = minimum sn
  let sn' = map (\x -> x - m) sn
  if length (filter (\x -> x > 1) sn') > 0
    then putStrLn "NO"
    else putStrLn "YES"
