import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

main = do
  [n, k, s] <- getIntList
  let t = if s > 10 ^ 5 then 1 else s + 1
  putStrLn $ concat (intersperse " " (map (\x -> show x) (replicate k s ++ replicate (n - k) t)))
