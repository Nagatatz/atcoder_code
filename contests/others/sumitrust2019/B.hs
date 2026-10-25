import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger

getInt = readInt <$> BS.getLine

rev :: Int -> Int -> Int
rev n n' | n'' < n' = rev (n+1) n'
         | n'' == n' = n
         | otherwise = -1
  where 
    n'' = n * 108 `div` 100 

main = do
  n <- getInt
  let n' = n * 100
  let d = n' `div` 108
  let m = n' `mod` 108
  if m == 0 then
    print d
  else do
    let x = rev d n
    if x == -1 then
      putStrLn ":("
    else
      print x