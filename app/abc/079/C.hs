import Control.Monad
import qualified Data.ByteString.Char8 as BS

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

display :: [(Int, Int)] -> String
display [] = ""
display (z : zs)
  | snd z == 1 = "+" ++ show (fst z) ++ display zs
  | otherwise = "-" ++ show (fst z) ++ display zs

calc :: Int -> [Int] ->[[Int]] -> String
calc a ns (st : sts)
  | a + sum(map (\x -> fst x * snd x) zs) == 7 = show a ++ display zs ++ "=7"
  | otherwise = calc a ns sts
  where
    zs = zip ns st

main = do
  [s] <- getString
  let [a, b, c, d] = map (\n -> read [n] :: Int) s
  let sts = replicateM 3 [1, (-1)]
  putStrLn $ calc a [b, c, d] sts
