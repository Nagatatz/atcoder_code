import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

search :: (Float, Float) -> (Float -> Bool) -> (Float, Float)
search (l, r) f
  | abs (l - r) < 0.001 = (l, r)
  | f m = search (l, m) f
  | otherwise = search (m, r) f
  where
    m = (l + r) / 2

main = do
  n <- getInt
  print $ fst $ search (0, 100) (\m -> m ^ 3 + m >= fromIntegral n)
