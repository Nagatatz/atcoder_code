import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.Maybe
import qualified Data.Vector.Unboxed   as VU
import qualified Data.Vector.Unboxed.Mutable as VUM


readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

override table i j = do
  v <- VUM.read table i
  VUM.write table i (v + 1)
  v' <- VUM.read table j
  VUM.write table j (v' - 1)

simulate' s i j k n as table = do
  let s' = as VU.! j + s
  if s' == k
    then do
      override table i (j + 1)
      return s'
    else do
      let isLast = j == n
      if s' < k && isLast
        then do
          override table i (j + 1)
          return s'
        else if s' > k || isLast
          then do
            override table i j
            return s'
          else do
            simulate' s' i (j + 1) k n as table

simulate s i j k n as table = do
  s' <- simulate' s i j k n as table
  when (i /= n) $  simulate 0 (i + 1) (i + 1) k n as table


main = do
  [n, k] <- getIntList
  as <- getIntList
  let as' = VU.fromList (0 : as)
  table <- VUM.replicate (n + 2) 0 :: IO (VUM.IOVector Int)
  simulate 0 1 1 k n as' table
  t <- VU.freeze table
  print $ VU.foldl (+) 0 $ VU.scanl (+) 0 t
