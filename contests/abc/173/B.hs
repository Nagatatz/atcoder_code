import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readString = map BS.unpack . BS.words

getInt = readInt <$> BS.getLine
getNString n = map readString <$> replicateM n BS.getLine

calc :: [String] -> Int -> Int -> Int -> Int -> [Int]
calc [] ac wa tle re = [ac, wa, tle, re]
calc (s : ss) ac wa tle re
  | s == "AC" = calc ss (ac + 1) wa tle re
  | s == "WA" = calc ss ac (wa + 1) tle re
  | s == "TLE" = calc ss ac wa (tle + 1) re
  | s == "RE" = calc ss ac wa tle (re + 1)

main = do
  n <- getInt
  ss <- getNString n
  let ss' = map head ss
  let [ac, wa, tle, re] = calc ss' 0 0 0 0
  putStrLn $ "AC x " ++ show ac
  putStrLn $ "WA x " ++ show wa
  putStrLn $ "TLE x " ++ show tle
  putStrLn $ "RE x " ++ show re
