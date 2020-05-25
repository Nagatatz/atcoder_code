import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

judge :: [Int] -> Int
judge [] = 0
judge [x] = 0
judge (x : y : xs)
  | x >= y = 1 + judge (y : xs)
  | otherwise = 0

tryMove :: [Int] -> [Int]
tryMove [] = []
tryMove (h : hs) = taken : tryMove hs'
  where
    taken = judge (h : hs)
    hs' = drop taken hs

main = do
  n <- getInt
  hs <- getIntList
  print $ maximum (tryMove hs)
