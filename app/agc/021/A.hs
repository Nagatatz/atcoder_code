import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

main = do
  n <- getInt
  let ns = show n
  let l = length ns
  if l == 1
    then print n
    else do
      let d = read [(head ns)] :: Int
      let m = (n + 1) `mod` 10
      if m == 0
        then print $ (l -1) * 9 + d
        else print $ (l -1) * 9 + (d - 1)
