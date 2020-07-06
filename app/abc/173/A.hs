import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

main = do
  n <- getInt
  let n' = n `mod` 1000
  if n' == 0 then print 0 else print $ 1000 - n'
