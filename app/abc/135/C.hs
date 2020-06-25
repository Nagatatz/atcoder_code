import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

calc :: [Int] -> [Int] -> Int -> Int -> Int
calc [] [] _ s = s
calc [a] [] b' s = calc [] [] 0 (s + minimum [a, b'])
calc (a : as) (b : bs) b' s = calc as bs b'' s'
  where
    a' = minimum [a, b']
    ax = a - a'
    bx = minimum [ax, b]
    b'' = b - bx
    s' = s + a' + bx

main = do
  n <- getInt
  as <- getIntList
  bs <- getIntList
  print $ calc as bs 0 0
