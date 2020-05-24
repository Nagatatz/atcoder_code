import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Char
import qualified Data.Vector.Unboxed.Mutable as VUM

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

readWords alphabets (w : ws) = do
  let position = ord w - ord 'a'
  count <- VUM.read alphabets position
  VUM.write alphabets position (count + 1)
  when (ws /= []) $ readWords alphabets ws

checkCount alphabets n = do
  count <- VUM.read alphabets n
  if odd count
    then putStrLn "No"
    else
      if n < 25
        then checkCount alphabets (n + 1)
        else putStrLn "Yes"

main = do
  [w] <- getString
  alphabets <- VUM.replicate 26 0 :: IO (VUM.IOVector Int)
  readWords alphabets w
  checkCount alphabets 0
