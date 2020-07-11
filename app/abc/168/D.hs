import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

-- 双方向化
setEdge :: VM.IOVector [Int] -> [[Int]] -> IO ()
setEdge v (ab : abs) = do
  let [a, b] = ab
  VM.modify v (a :) b
  VM.modify v (b :) a
  when (abs /= []) $ setEdge v abs

-- table個別埋め
draw :: VUM.IOVector Int -> [Int] -> Int -> [Int] -> IO [Int]
draw table (e : es) n ks = do
  p <- VUM.read table (e - 1)
  when (p == 0) $ VUM.write table (e - 1) n
  let ks' = if (p == 0) then (e : ks) else ks
  if (es /= [])
    then draw table es n ks'
    else return ks'

-- 埋める
simulate :: [Int] -> [Int] -> VUM.IOVector Int -> V.Vector [Int] -> IO ()
simulate (n : ns) ks table edges = do
  let edges' = edges V.! n
  ks' <- draw table edges' n ks
  if (ns /= [])
    then simulate ns ks' table edges
    else when (ks' /= []) $ simulate ks' [] table edges

main = do
  [n, m] <- getIntList
  abs <- getIntNList m
  vm <- VM.replicate (n + 1) [] :: IO (VM.IOVector [Int])
  setEdge vm abs
  edges <- V.freeze vm
  table <- VUM.replicate n 0 :: IO (VUM.IOVector Int)
  simulate [1] [] table edges
  t <- VU.freeze table
  putStrLn "Yes"
  VU.mapM_ print (VU.tail t)
