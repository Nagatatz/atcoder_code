import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine

main = do
  [x1, y1, x2, y2] <- getIntList
  let dx = x2 - x1
  let dy = y2 - y1
  let x3 = x2 - dy
  let y3 = y2 + dx
  let x4 = x1 - dy
  let y4 = y1 + dx
  putStrLn $ unwords (map show [x3, y3, x4, y4])
