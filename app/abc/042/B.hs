import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.List

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
readString = map BS.unpack . BS.words

getIntList = readIntList <$> BS.getLine
getNString n = map readString <$> replicateM n BS.getLine

main = do
  [n, l] <- getIntList
  ss <- getNString n
  let ss' = map head ss
  putStrLn $ concat (sort ss')
