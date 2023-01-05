import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

main = do
  x <-getInt
  print $ x ^ 2