import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

calc :: String -> String -> Bool
calc [] [] = False
calc _ [] = False
calc [] _ = True
calc (s : ss) (t : ts)
  | s == t = calc ss ts
  | sc > tc = False
  | sc < tc = True
  where
    sc = ord s
    tc = ord t

main = do
  [s] <- getString
  [t] <- getString
  let s' = sort s
  let t' = reverse (sort t)
  let tf = calc s' t'
  if tf then putStrLn "Yes" else putStrLn "No"
