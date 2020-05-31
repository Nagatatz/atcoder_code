-- WA
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Control.Monad

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

main = do
  [a, b] <- getString
  let a' =  read a :: Integer
  let b' =  floor((read b :: Float) * 100)
  let b1 = b' `div` 100
  let b2 = (b' - b1 * 100) `div` 10
  let b3 = (b' - b1 * 100 - b2 * 10)  
  print $ a' * b1 + (a' * b2 * 10 + a' * b3) `div` 100