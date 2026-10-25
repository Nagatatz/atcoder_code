import qualified Data.ByteString.Char8 as BS
import           Data.Char             (digitToInt)
import           Numeric               (readInt)

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

main = do
  [n] <- getString
  print $ fst $ head $ Numeric.readInt 2 (`elem` "01") digitToInt n
