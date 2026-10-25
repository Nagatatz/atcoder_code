import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger

readString = map BS.unpack . BS.words

getInt = readInt <$> BS.getLine

getString = readString <$> BS.getLine

calc :: String -> Int
calc [] = 0
calc [c'] = 0
calc s
  | c' == c'' = 1 + calc s''
  | otherwise = calc s'
  where
    c' = head s
    s' = tail s
    c'' = head s'
    s'' = tail s'

main = do
  [s] <- getString
  k <- getInt
  if all (== head s) s
    then print $ (length s) * k `div` 2
    else do
      if last s /= head s
        then print $ (calc s) * k
        else do
          let d1 = calc s
          let d2 = calc (s ++ s)
          let diff = d2 - d1
          print $ d1 + (k - 1) * diff
