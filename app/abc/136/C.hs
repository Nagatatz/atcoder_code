import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine

getIntList = readIntList <$> BS.getLine

simulate :: [Int] -> Int -> String
simulate [] b = "Yes"
simulate (h:hs) b | h - b >= 2 = simulate hs (h - 1)
                  | h - b == 1 || h - b == 0 = simulate hs b
                  | otherwise = "No"

main = do
  n <- getInt
  hs <- getIntList
  putStrLn $ simulate hs (head hs)
