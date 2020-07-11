import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

type Node = Int

type Edge = (Node, Node)

type Path = [Node]

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine

next :: Node -> [Edge] -> [Node]
next n = (map snd) . (filter ((n ==) . fst))

-- build paths by breadth first
_byB :: MonadPlus m => Node -> [Edge] -> [Path] -> m Path
_byB _ _ [] = mzero
_byB n es (p : ps)
  | (p !! 0) == n = (return p) `mplus` (_byB n es ps)
  | otherwise = _byB n es $ ps ++ (map (: p) $ next (p !! 0) es)

-- list first path by Maybe
firstPath ::
  (Node -> [Edge] -> [Path] -> Maybe Path) ->
  Node ->
  Node ->
  [Edge] ->
  Maybe Path
firstPath f p0 p1 es = fmap reverse $ f p1 es [[p0]]

createEdge [] = []
createEdge (ab : abs) = (a, b) : (b, a) : createEdge abs
  where
    [a, b] = ab

main = do
  [n, m] <- getIntList
  abs <- getIntNList m
  let edges = createEdge abs
  putStrLn "Yes"
  mapM_ print (map (\n' -> last (init (fromJust (firstPath _byB 1 n' edges)))) [2 .. n])
