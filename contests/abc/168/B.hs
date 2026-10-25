import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger

getInt = readInt <$> BS.getLine
getString = BS.unpack <$> BS.getLine

summary :: String -> Int -> String
summary [] 0 = []
summary _  0 = "..."
summary [] _ = []
summary (x:xs) k = x : summary xs (k-1)

main = do
  k <- getInt
  s <- getString
  putStrLn $ summary s k
