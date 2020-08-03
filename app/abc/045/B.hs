import qualified Data.ByteString.Char8 as BS

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

simulate :: String -> String -> String -> Char -> String
simulate [] _ _ 'a' = "A"
simulate _ [] _ 'b' = "B"
simulate _ _ [] 'c' = "C"
simulate as bs cs x
  | x == 'a' = simulate (tail as) bs cs (head as)
  | x == 'b' = simulate as (tail bs) cs (head bs)
  | x == 'c' = simulate as bs (tail cs) (head cs)

main = do
  [as] <- getString
  [bs] <- getString
  [cs] <- getString
  putStrLn $ simulate as bs cs 'a'
