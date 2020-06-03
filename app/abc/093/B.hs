import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

printNum (x : xs) = do
  print x
  when (xs /= []) $ printNum xs

main = do
  [a, b, k] <- getIntList
  let xs = filter (\x -> (x < (a + k)) || (x > (b - k))) [a .. b]
  printNum xs
