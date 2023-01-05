import qualified Data.ByteString.Char8 as BS
import           Data.Char             (intToDigit)
import           Data.Maybe
import           Numeric               (showIntAtBase)

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

padding10 :: String -> String
padding10 s
  | n > 0 = replicate n '0' ++ s
  | otherwise = s
  where
    n = 10 - length s

main = do
  n <- getInt
  putStrLn $ padding10 $ showIntAtBase 2 intToDigit n ""
