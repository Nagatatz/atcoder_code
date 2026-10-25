import Control.Monad
import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

readString = map BS.unpack . BS.words

getIntList = readIntList <$> BS.getLine

getNString n = map readString <$> replicateM n BS.getLine

simulate :: Array (Int, Int) Char -> (Int, Int) -> (Int, Int) -> String
simulate sss p p'
  | left && up = "Impossible"
  | right && down = "Impossible"
  | p == p' = "Possible"
  | right = simulate sss (h, w + 1) p'
  | down = simulate sss (h + 1, w) p'
  | otherwise = "Impossible"
  where
    (h, w) = p
    left = sss ! (h, w - 1) == '#'
    right = sss ! (h, w + 1) == '#'
    up = sss ! (h - 1, w) == '#'
    down = sss ! (h + 1, w) == '#'

main = do
  [h, w] <- getIntList
  sss <- getNString h
  let dots = replicate (w + 2) "."
  let sss' = concat (dots ++ (map (\x -> "." ++ x ++ ".") (map head sss)) ++ dots)
  let sss'' = listArray ((0, 0), (h + 1, w + 1)) sss'
  putStrLn $ simulate sss'' (1, 1) (h, w)
