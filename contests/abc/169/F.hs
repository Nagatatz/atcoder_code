-- TLE
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.Array
import Data.List
import Control.Monad

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

powerset :: [a] -> [[a]]  
powerset xs = filterM (\x -> [True, False]) xs

createWeight [] as = []
createWeight (s:ss) as = as ! s : createWeight ss as

calc [] as s = 0
calc (ss:sss) as s = k + calc sss as s
  where
    weights = createWeight ss as
    k = length (filter (==s) (map sum (powerset weights)))

main = do
  [n, s] <- getIntList
  as <- getIntList
  let as' = listArray (1, length as) as
  let sss = init (powerset [1..n])
  print $ (calc sss as' s) `mod` 998244353
                                   
