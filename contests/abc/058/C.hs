import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readString = map BS.unpack . BS.words

getInt = readInt <$> BS.getLine

getNString n = concat . map readString <$> replicateM n BS.getLine

calc :: String -> String -> String
calc _ [] = ""
calc [] _ = ""
calc (c : s) (a : ans)
  | c == a = a : (calc s ans)
  | c > a = calc (c : s) ans
  | c < a = calc s (a : ans)

simulate :: [String] -> String -> String
simulate [] ans = ans
simulate (s : ss) ans = simulate ss (calc s ans)

main = do
  n <- getInt
  ss <- getNString n
  let ss' = map sort ss
  putStrLn $ simulate (tail ss') (head ss')
