import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInteger

getInt = readInt <$> BS.getLine

main = do
  n <- getInt
  print $ product [1 .. n] `mod` (10 ^ 9 + 7)
