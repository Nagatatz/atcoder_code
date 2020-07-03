import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine
getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

simulate (sc : scs) table = do
  let i = head sc
  let v = last sc
  p <- VUM.read table (i -1)
  if p == (-1)
    then do
      VUM.write table (i -1) v
      if scs /= []
        then simulate scs table
        else return True
    else do
      if p /= v
        then return False
        else do
          if scs /= []
            then simulate scs table
            else return True

showL table i n f s = do
  v <- VUM.read table (i -1)
  if f == False && (v == 0 || v == (-1))
    then do
      if v == 0
        then do
          if n == 1
            then return "0"
            else return "-1"
        else showL table (i + 1) n True "1"
    else do
      let v'
            | v == (-1) = "0"
            | otherwise = show v
      let s' = v' ++ s
      if i == n
        then return (reverse s')
        else showL table (i + 1) n True s'

main = do
  [n, m] <- getIntList
  if m == 0
    then
      if n == 1
        then print 0
        else putStrLn $ "1" ++ (replicate (n - 1) '0')
    else do
      scs <- getIntNList m
      table <- VUM.replicate n (-1) :: IO (VUM.IOVector Int)
      result <- simulate scs table
      if result == False
        then print (-1)
        else do
          num <- showL table 1 n False ""
          putStrLn num
