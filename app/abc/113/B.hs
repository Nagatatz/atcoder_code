import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine

getIntList = readIntList <$> BS.getLine

temp :: Int -> Int -> Int -> Float
temp a h t = abs (((fromIntegral a) :: Float) - (((fromIntegral t) :: Float) - ((fromIntegral h) :: Float) * 0.006))

calc :: Array Int Int -> Int -> Int -> Float -> Int -> Int -> Int
calc hs t a a' 1 n'
  | a'' < a' = 1
  | otherwise = n'
  where
    a'' = temp a (hs ! 1) t
calc hs t a (-1) n (-1)
  | a'' == 0 = n
  | otherwise = calc hs t a a'' (n -1) n
  where
    a'' = temp a (hs ! n) t
calc hs t a a' n n'
  | a'' == 0 = n
  | a'' < a' = calc hs t a a'' (n -1) n
  | otherwise = calc hs t a a' (n -1) n'
  where
    a'' = temp a (hs ! n) t

main = do
  n <- getInt
  [t, a] <- getIntList
  hs <- getIntList
  let hs' = listArray (1, length hs) hs
  print $ calc hs' t a (-1 :: Float) n (-1)
