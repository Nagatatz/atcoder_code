import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import qualified Data.Vector.Unboxed as VU

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

calc :: VU.Vector Int -> VU.Vector Int
calc as = do
  let n = VU.length as
  let base = VU.generate (n + 1) (\x -> 0)
  let ds = VU.imap (\i a -> (maximum [0, i - a], 1)) as VU.++ VU.imap (\i a -> (minimum [n, i + a + 1], -1)) as
  VU.postscanl' (+) 0 (VU.init (VU.accumulate (+) base ds))

judge :: VU.Vector Int -> Int -> Int -> VU.Vector Int
judge as n 0 = as
judge as n k
  | VU.all (== n) as = as
  | otherwise = judge (calc as) n (k -1)

main = do
  [n, k] <- getIntList
  as <- getIntList
  let as' = VU.fromList as
  let as'' = judge as' n k
  mapM_ putStr (map (\x -> show x ++ " ") (VU.toList as''))
