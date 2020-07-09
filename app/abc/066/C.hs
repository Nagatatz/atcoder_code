import Control.Monad
import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine

getIntList = readIntList <$> BS.getLine

simulate as table i' i l o h = do
  VUM.write table i (as ! i')
  let i''
        | o == True && i < h = i' - 2
        | o == True && i == h = i' + 1
        | o == True && i > h = i' + 2
        | o == False && i < h - 1 = i' - 2
        | o == False && i == h - 1 = i' - 1
        | o == False && i > h - 1 = i' + 2
  when (i /= l - 1) $ simulate as table i'' (i + 1) l o h

main = do
  n <- getInt
  as <- getIntList
  let l = length as
  let as' = listArray (1, l) as
  table <- VUM.replicate n 0 :: IO (VUM.IOVector Int)
  simulate as' table l 0 l (odd l) (l `div` 2)
  v <- VU.freeze table
  putStrLn $ unwords (map show (VU.toList v))
