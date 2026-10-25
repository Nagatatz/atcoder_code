import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger

getInt = readInt <$> BS.getLine

checkNum :: Int -> String
checkNum d
  | n `elem` [2, 4, 5, 7, 9] = "hon"
  | n `elem` [0, 1, 6, 8] = "pon"
  | otherwise = "bon"
  where
    n = d `mod` 10

main = do
  n <- getInt
  putStrLn $ checkNum n
