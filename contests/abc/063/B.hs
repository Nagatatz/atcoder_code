import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

seek :: String -> Bool
seek [s] = True
seek (s : ss)
  | s `elem` ss = False
  | otherwise = seek ss

main = do
  [s] <- getString
  if seek s
    then putStrLn "yes"
    else putStrLn "no"
