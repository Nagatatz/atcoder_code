import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.List

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

seek :: String -> Char -> Maybe Int
seek s c = elemIndex c s

main = do
  [s] <- getString
  print $ (length s - fromJust (seek (reverse s) 'Z')) - fromJust (seek s 'A')

