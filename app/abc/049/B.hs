import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
readString = map BS.unpack . BS.words

getIntList = readIntList <$> BS.getLine
getNString n = map readString <$> replicateM n BS.getLine

display (cs : css) = do
  putStrLn $cs
  putStrLn $cs
  when (css /= []) $ display css

main = do
  [h, w] <- getIntList
  css <- getNString h
  let css' = map head css
  display css'
