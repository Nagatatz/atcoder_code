import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fst . fromJust . BS.readInt

getIntListN n = map readInt <$> replicateM (fromIntegral n) BS.getLine

calc :: [Int] -> Bool -> Int
calc [] lo = 0
calc (x : xs) lo
  | x `mod` 10 == 0 = x + calc xs lo
  | lo = x + calc xs False
  | otherwise = ((x + 9) `div` 10) * 10 + calc xs lo

main = do
  xs <- getIntListN 5
  let xs' = sortBy (\x y -> compare (x `mod` 10) (y `mod` 10)) xs
  print $ calc xs' True
