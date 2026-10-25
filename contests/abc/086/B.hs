import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readString = map BS.unpack . BS.words
getString = readString <$> BS.getLine

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

main = do
  [a, b] <- getString
  let x = read (a ++ b) :: Int
  let x_sqrt = isqrt x
  if x_sqrt ^ 2 == x then
    putStrLn "Yes"
  else
    putStrLn "No"