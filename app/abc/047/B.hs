import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine

getIntList = readIntList <$> BS.getLine

getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

paint wmin wmax hmin hmax [] = (wmax - wmin) * (hmax - hmin)
paint wmin wmax hmin hmax (xya : xyas)
  | a == 1 && x <= wmin = paint wmin wmax hmin hmax xyas
  | a == 1 && x > wmin && x < wmax = paint x wmax hmin hmax xyas
  | a == 1 && x >= wmax = 0
  | a == 2 && x <= wmin = 0
  | a == 2 && x > wmin && x < wmax = paint wmin x hmin hmax xyas
  | a == 2 && x >= wmax = paint wmin wmax hmin hmax xyas
  | a == 3 && y <= hmin = paint wmin wmax hmin hmax xyas
  | a == 3 && y > hmin && y < hmax = paint wmin wmax y hmax xyas
  | a == 3 && y >= hmax = 0
  | a == 4 && y <= hmin = 0
  | a == 4 && y > hmin && y < hmax = paint wmin wmax hmin y xyas
  | a == 4 && y >= hmax = paint wmin wmax hmin hmax xyas
  where
    [x, y, a] = xya

main = do
  [w, h, n] <- getIntList
  xyas <- getIntNList n
  print $ paint 0 w 0 h xyas
