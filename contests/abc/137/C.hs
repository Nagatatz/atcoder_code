import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readString = map BS.unpack . BS.words

getInt = readInt <$> BS.getLine

getNString n = concat . map readString <$> replicateM n BS.getLine

getS :: [String] -> Integer
getS [] = 0
getS (s : ss) = n + getS ts
  where
    (ss', ts) = span (== s) ss
    k = fromIntegral (length ss') :: Integer
    n = (k * (k + 1)) `div` 2

main = do
  n <- getInt
  ss <- getNString n
  let ss' = sort (map (\x -> sort x) ss)
  print $ getS ss'
