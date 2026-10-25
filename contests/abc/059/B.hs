import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.Char

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

judge :: String -> String -> String
judge [] [] = "EQUAL"
judge (x : xs) (y : ys)
  | ord x > ord y = "GREATER"
  | ord x < ord y = "LESS"
  | otherwise = judge xs ys

main = do
  [a] <- getString
  [b] <- getString
  let la = length a
  let lb = length b
  let x | la > lb = "GREATER" | la < lb = "LESS" | otherwise = judge a b
  putStrLn x
