import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readString = map BS.unpack . BS.words

readInt = fst . fromJust . BS.readInt

getString = readString <$> BS.getLine

getInt = readInt <$> BS.getLine

incDec :: String -> Int -> Int -> Int
incDec [] m x = m
incDec (s : ss) m x = incDec ss m' x'
  where
    x' =
      if s == 'I'
        then x + 1
        else x - 1
    m' = maximum [x', m]

main = do
  n <- getInt
  [s] <- getString
  print $ incDec s 0 0
