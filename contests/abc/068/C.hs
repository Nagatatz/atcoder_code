import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

calc table n (ab : abs) = do
  let [h, l] = ab
  let k = if h == 1 || h == n then l else if l == 1 || l == n then h else 0
  if k == 0
    then do
      if abs /= []
        then calc table n abs
        else return "IMPOSSIBLE"
    else do
      p <- VUM.read table k
      if p == 1
        then return "POSSIBLE"
        else do
          VUM.write table k 1
          if abs /= []
            then calc table n abs
            else return "IMPOSSIBLE"

main = do
  [n, m] <- getIntList
  abs <- getIntNList m
  table <- VUM.replicate (n + 1) 0 :: IO (VUM.IOVector Int)
  result <- calc table n abs
  putStrLn result
