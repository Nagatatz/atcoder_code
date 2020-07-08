import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

main = do
  [k, a, b] <- getIntList
  if a >= (b - 2)
    then print $ k + 1
    else do
      let i = a - 1
      if k <= i
        then print $ k + 1
        else do
          let r = k - i
          let gain = b - a
          print $ a + r `div` 2 * gain + r `mod` 2
