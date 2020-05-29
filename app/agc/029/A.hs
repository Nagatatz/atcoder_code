import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

calc :: String -> Int
calc [] = 0
calc ss = k + calc ss'''
  where
    len_ss = length ss
    ss' = takeWhile (== 'B') ss
    len_ss' = length ss'
    ss'' = dropWhile (== 'B') ss
    ss''' =
      if ss'' /= []
        then replicate len_ss' 'B' ++ tail ss''
        else []
    k =
      if len_ss /= len_ss'
        then len_ss'
        else 0

main = do
  [s] <- getString
  print $ calc s
