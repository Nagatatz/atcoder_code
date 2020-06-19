import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

simulate table k a b c = do
  if k == c
    then return "YES"
    else do
      p <- VUM.read table k
      if p /= 0
        then return "NO"
        else do
          VUM.write table k 1
          simulate table ((k + a) `mod` b) a b c

main = do
  [a, b, c] <- getIntList
  table <- VUM.replicate b 0 :: IO (VUM.IOVector Int)
  sentense <- simulate table (a `mod` b) a b c
  putStrLn sentense
