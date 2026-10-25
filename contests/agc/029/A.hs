import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

calc :: String -> Int -> Int
calc [] cur = 0
calc (s : ss) cur
  | s == 'W' = cur + calc ss cur
  | otherwise = calc ss (cur + 1)

main = do
  [s] <- getString
  print $ calc s 0
