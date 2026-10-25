import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
import qualified Data.Set as Set

readInt = fst . fromJust . BS.readInt
readString = map BS.unpack . BS.words

getInt = readInt <$> BS.getLine
getString = readString <$> BS.getLine

ordNub :: Ord a => [a] -> [a]
ordNub xs =
  foldr
    ( \x k s ->
        if Set.member x s
          then k s
          else x : k (Set.insert x s)
    )
    (const [])
    xs
    Set.empty

comb' :: Int -> String -> String -> [String]
comb' _ [] _ = []
comb' 1 xs _ = [[x] | x <- (ordNub xs)]
comb' n (x : xs) ys
  | x `notElem` ys = [x : y | y <- comb' (n -1) xs []] ++ comb' n xs (x : ys)
  | otherwise = comb' n xs ys

main = do
  n <- getInt
  [s] <- getString
  print $ length (comb' 3 s "")
