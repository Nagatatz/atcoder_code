import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

simulate table a b c = do
  let s = ((a + b) ^ 2 + (b + c) ^ 2 + (c + a) ^ 2) `div` 2
  if s <= 10000
    then do
      p <- VUM.read table s
      if a == b && b == c
        then VUM.write table s (p + 1)
        else
          if a == b || b == c
            then VUM.write table s (p + 3)
            else VUM.write table s (p + 6)
      simulate table a b (c + 1)
    else do
      let s' = ((a + b + 1) ^ 2 + (b + b + 1) ^ 2 + (b + 1 + a) ^ 2) `div` 2
      if s' <= 10000
        then simulate table a (b + 1) (b + 1)
        else do
          let s'' = (a * 2 + 1) ^ 2 `div` 2
          when (s'' <= 10000) $ simulate table (a + 1) (a + 1) (a + 1)

main = do
  n <- getInt
  let n2 = [1 .. 100]
  table <- VUM.replicate 100001 0 :: IO (VUM.IOVector Int)
  simulate table 1 1 1
  vu <- VU.freeze table
  VU.mapM_ print (VU.take n (VU.tail vu))
