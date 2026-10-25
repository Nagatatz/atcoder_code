import qualified Data.ByteString.Char8 as BS
import Data.Maybe

getString = BS.unpack <$> BS.getLine

findACGT :: String -> [Int]
findACGT [] = []
findACGT ss = length (fst spanned) : findACGT (snd spanned)
  where
    dropped = dropWhile (`notElem` "ACGT") ss
    spanned = span (`elem` "ACGT") dropped

main = do
  ss <- getString
  print $ maximum (findACGT ss)
