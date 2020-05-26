import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Control.Monad

readString = map BS.unpack . BS.words
readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getString = readString <$> BS.getLine
getIntList = readIntList <$> BS.getLine
getNString n = map readString <$> replicateM n BS.getLine

pp (ss:sss) = do
  putStrLn $ "#" ++ concat ss ++ "#"
  when (sss /= []) $ pp sss

main = do
  [h, w] <- getIntList
  sss <- getNString h
  let line = replicate (w + 2) '#'
  putStrLn line
  pp sss
  putStrLn line
