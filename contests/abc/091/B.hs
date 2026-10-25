import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

readInt = fst . fromJust . BS.readInt
readString = map BS.unpack . BS.words

getInt = readInt <$> BS.getLine
getNString n = map readString <$> replicateM n BS.getLine

check :: [String] -> [String] -> Int -> Int
check [] _ m = m
check ss ts m = check (snd ss') (snd ts') m''
  where
    s = head ss
    ss' = partition (== s) ss
    ts' = partition (== s) ts
    m' = length (fst ss') - length (fst ts')
    m'' = if m' > m then m' else m

main = do
  n <- getInt
  ss <- getNString n
  m <- getInt
  ts <- getNString m
  let ss' = map head ss
  let ts' = map head ts
  print $ check ss' ts' 0
