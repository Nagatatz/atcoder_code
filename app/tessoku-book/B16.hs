import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

simulate :: (Int, Int) -> (Int, Int) -> [Int] -> Int
simulate m' m'' [] = fst m'
simulate m' m'' (h : hs) = simulate (r, h) m' hs
  where
    r = minimum $ map (\m -> fst m + abs (h - snd m)) [m', m'']

main = do
  n <- getInt
  hs <- getIntList
  let h'' = head hs
  let hs' = tail hs
  let h' = head hs'
  let hs'' = tail hs'
  let x = abs (h' - h'')
  print $ simulate (x, h') (0, h'') hs''
