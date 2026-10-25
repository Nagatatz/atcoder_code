import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap           as IM
import           Data.List
import           Data.Maybe
import qualified Data.Set              as Set

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

main = do
  n <- getInt
  as <- getIntList
  let as' = Set.fromList as
  let l = Set.size as'
  let m = IM.fromList (zip (Set.toList as') [1 .. l])
  putStrLn $ unwords (map (\a -> show (m IM.! a)) as)
