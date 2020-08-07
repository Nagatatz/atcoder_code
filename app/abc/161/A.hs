import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

main = do
  [x, y, z] <- getIntList
  putStrLn $ (show z) ++ " " ++ (show x) ++ " " ++ (show y)
