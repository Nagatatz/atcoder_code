import           Control.Monad
import qualified Data.ByteString.Char8       as BS
import           Data.List
import           Data.Maybe
import qualified Data.Set                    as Set
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

search' :: (Int, Int) -> (Int -> Bool) -> (Int, Int)
search' (l, r) f
  | l >= r = (l, r)
  | f m = search' (l, m) f
  | otherwise = search' (m + 1, r) f
  where
    m = (l + r) `div` 2

search i l (a : as) as' table = do
  let j = fst (search' (0, l) (\m -> (Set.elemAt m as') >= a))
  VUM.write table i (j + 1)
  when (as /= []) $ search (i + 1) l as as' table

main = do
  n <- getInt
  as <- getIntList
  let as' = Set.fromList as
  let l = Set.size as'
  table <- VUM.replicate (length as) 0 :: IO (VUM.IOVector Int)
  search 0 (l - 1) as as' table
  t <- VU.freeze table
  putStrLn $ unwords (map show (VU.toList t))
