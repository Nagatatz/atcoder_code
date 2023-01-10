import           Control.Monad
import qualified Data.ByteString.Char8       as BS
import           Data.List
import           Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

getPosition :: Int -> Int -> Int -> Int
getPosition y x w = y * (w + 1) + x

setRow (x : xs) xss i v = do
  p <- VUM.read v (i - 1)
  VUM.write v i (p + x)
  if xs /= []
    then setRow xs xss (i + 1) v
    else when (xss /= []) $ setRow (head xss) (tail xss) (i + 2) v

setColumn i j w l v = do
  p <- VUM.read v (i - w - 1)
  p' <- VUM.read v i
  VUM.write v i (p + p')
  if i + w + 1 < l
    then setColumn (i + w + 1) j w l v
    else when (j /= w + 1) $ setColumn (j + w + 1) (j + 1) w l v

getValues ([a, b, c, d] : abcds) w v = do
  p <- VUM.read v (getPosition c d w)
  p' <- VUM.read v (getPosition (a -1) (b -1) w)
  p'' <- VUM.read v (getPosition (a -1) d w)
  p''' <- VUM.read v (getPosition c (b -1) w)
  print $ p + p' - p'' - p'''
  when (abcds /= []) $ getValues abcds w v

printVector i l v = do
  p <- VUM.read v i
  print p
  when (i /= (l - 1)) $ printVector (i + 1) l v

main = do
  [h, w] <- getIntList
  xss <- getIntNList h
  q <- getInt
  abcds <- getIntNList q
  let l = (w + 1) * (h + 1)
  v <- VUM.replicate (l) 0 :: IO (VUM.IOVector Int)
  setRow (head xss) (tail xss) (w + 2) v
  setColumn (w + 2) 2 w l v
  getValues abcds w v
