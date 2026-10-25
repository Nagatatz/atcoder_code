import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine

getIntList = readIntList <$> BS.getLine

main = do
  [a, b, c, d] <- getIntList
  let cl = b `div` c - (a -1) `div` c
  let dl = b `div` d - (a -1) `div` d
  let l = lcm c d
  let ll = b `div` l - (a -1) `div` l
  print $ (b - a + 1) - (cl + dl - ll)
