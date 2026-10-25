import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readString = map BS.unpack . BS.words

getString = readString <$> BS.getLine

check :: String -> String -> String
check [] as = as
check (s : ss) as = check ss as'
  where
    as'
      | s `elem` as = as
      | otherwise = s : as

main = do
  [s] <- getString
  let setS = check s ""
  let lenS = length (setS)
  let result
        | odd lenS = "No"
        | lenS == 4 = "Yes"
        | or [and ['N' `elem` setS, 'S' `elem` setS], and ['W' `elem` setS, 'E' `elem` setS]] = "Yes"
        | otherwise = "No"
  putStrLn result
