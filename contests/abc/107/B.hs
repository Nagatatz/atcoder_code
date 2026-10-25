import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
readString = map BS.unpack . BS.words

getIntList = readIntList <$> BS.getLine
getNString n = map readString <$> replicateM n BS.getLine

compressRow :: [[Char]] -> [[Char]] -> [[Char]]
compressRow [] css' = reverse css'
compressRow (cs : css) css'
  | all (== '.') cs = compressRow css css'
  | otherwise = compressRow css (cs : css')

main = do
  [h, w] <- getIntList
  css <- getNString h
  let css' = map head css
  mapM_ putStrLn (transpose (compressRow (transpose (compressRow css' [])) []))
