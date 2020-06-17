import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

main = do
  x <- getInt
  let d = x `div` 100
  let m = x `mod` 100
  if m <= 5 * d
    then print 1
    else print 0
