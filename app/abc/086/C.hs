import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

diff :: [Int] -> [Int] -> [Int]
diff a b = map (\(p, q) -> p - q) $ zip a b

calc :: [[Int]] -> String
calc (txy : []) = "Yes"
calc (txy : txy' : txys)
  | result == True = calc (txy' : txys)
  | otherwise = "No"
  where
    [t, x, y] = diff txy' txy
    d = t - (abs x + abs y)
    result =
      if d < 0 || odd d
        then False
        else True

main = do
  n <- getInt
  txys <- getIntNList n
  let txys' = [0, 0, 0] : txys
  putStrLn $ calc txys'
