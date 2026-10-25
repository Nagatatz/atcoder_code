import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

paint' table n i = do
  p <- VUM.read table (n - 2)
  when (p == 0) $ VUM.write table (n - 2) i

paint :: VUM.IOVector Int -> V.Vector (Int, Int) -> [Int] -> [Int] -> IO (V.Vector (Int, Int), [Int])
paint table v (i : is) js = do
  let (xs, ys) = V.partition (\(a, b) -> a == i) v
  let ys' = V.filter (\(a, b) -> b /= i) ys
  let js' = V.map (\(a, b) -> b) xs
  V.mapM_ (\n -> paint' table n i) js'
  let js'' = js ++ (V.toList js')
  if is /= []
    then paint table ys' is js''
    else return (ys', js'')

createLine :: [[Int]] -> [(Int, Int)]
createLine [] = []
createLine (ab : abs) = (a, b) : (b, a) : createLine abs
  where
    [a, b] = ab

padding :: VUM.IOVector Int -> V.Vector (Int, Int) -> [Int] -> IO ()
padding table v is = do
  (v', js) <- paint table v is []
  t <- VU.freeze table
  when (v' /= V.empty && VU.any (== 0) t) $ padding table v' js

main = do
  [n, m] <- getIntList
  abs <- getIntNList m
  let lines = createLine abs
  let v = V.fromList lines
  table <- VUM.replicate (n - 1) 0 :: IO (VUM.IOVector Int)
  padding table v [1]
  t <- VU.freeze table
  if VU.length (VU.filter (== 0) t) /= 0
    then putStrLn "No"
    else do
      putStrLn "Yes"
      VU.mapM_ print t
