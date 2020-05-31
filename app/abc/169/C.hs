import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

main = do
  [a, b] <- getString
  let a' = read a :: Integer
  let b' = read (delete '.' b) :: Integer
  print $ (a' * b') `div` 100
