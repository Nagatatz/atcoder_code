import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

main = do
  [w, h, x, y] <- getIntList
  let s = ((fromIntegral w :: Double) * (fromIntegral h :: Double)) / 2
  if (fromIntegral w :: Double) / 2 == (fromIntegral x :: Double) && (fromIntegral h :: Double) / 2 == (fromIntegral y :: Double)
    then putStrLn $ unwords [show s, "1"]
    else putStrLn $ unwords [show s, "0"]
