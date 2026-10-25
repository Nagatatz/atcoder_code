import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

main = do
  [n, k] :: [Int] <- getIntList
  let modp = n `mod` k
  if modp == 0
    then print 0
    else do
      let modp' = abs (modp - k)
      if modp < modp'
        then print modp
        else print modp'
