import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

angleDiffDeca :: Int -> Int -> Int
angleDiffDeca h m = abs (((5 * (60 * h + m)) `mod` 3600) - ((60 * m) `mod` 3600))

main = do
  [a, b, h, m] <- getIntList
  let angleDiff = angleDiffDeca h m
  let angle = fromIntegral angleDiff * pi / (1800 :: Double)
  print $ sqrt ((fromIntegral a - fromIntegral b * cos angle) ^ 2 + (fromIntegral b * sin angle) ^ 2)
