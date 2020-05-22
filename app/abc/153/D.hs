import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger

getInt = readInt <$> BS.getLine

play :: Int -> Int -> Int
play 1 k = k
play h k = k + play (h `div` 2) (2 * k)

main = do
  h <- getInt
  print $ play h 1
