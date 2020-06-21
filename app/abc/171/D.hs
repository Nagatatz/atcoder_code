import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine
getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

initial (a : as) table s = do
  let s' = s + a
  p <- VUM.read table a
  VUM.write table a (p + 1)
  if as == []
    then return s'
    else initial as table s'

simulate (bc : bcs) table s = do
  let b = head bc
  let c = last bc
  pb <- VUM.read table b
  VUM.write table b 0
  pc <- VUM.read table c
  VUM.write table c (pc + pb)
  let s' = s + pb * (c - b)
  print s'
  when (bcs /= []) $ simulate bcs table s'

main = do
  n <- getInt
  as <- getIntList
  q <- getInt
  bcs <- getIntNList q
  table <- VUM.replicate (10 ^ 5 + 1) 0 :: IO (VUM.IOVector Int)
  s' <- initial as table 0
  simulate bcs table s'
