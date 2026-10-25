import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List
import Data.Maybe

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

seek :: Char -> String -> String
seek 'z' s
  | 'z' `elem` s = "None"
  | otherwise = "z"
seek c s
  | c `elem` s = seek (chr (ord c + 1)) s
  | otherwise = [c]

main = do
  [s] <- getString
  let s' = sort (nub s)
  putStrLn $ seek 'a' s'
