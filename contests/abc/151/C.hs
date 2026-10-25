import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
readString = map BS.unpack . BS.words

getIntList = readIntList <$> BS.getLine
getNString n = map readString <$> replicateM n BS.getLine

simulate (ps : pss) table_ac table_wa ac wa = do
  let f = fst ps
  let s = snd ps
  p <- VUM.read table_ac f
  if p == 0
    then do
      if s == "AC"
        then do
          wa' <- VUM.read table_wa f
          let wa'' = wa + wa'
          VUM.write table_ac f 1
          let ac' = ac + 1
          if pss == [] then return (ac', wa'') else simulate pss table_ac table_wa ac' wa''
        else do
          wa' <- VUM.read table_wa f
          VUM.write table_wa f (wa' + 1)
          if pss == [] then return (ac, wa) else simulate pss table_ac table_wa ac wa
    else do
      if pss == [] then return (ac, wa) else simulate pss table_ac table_wa ac wa

main = do
  [n, m] <- getIntList
  pss <- getNString m
  if m == 0
    then putStrLn "0 0"
    else do
      let pss' = map (\ps -> ((read (head ps) :: Int), last ps)) pss
      table_ac <- VUM.replicate (n + 1) 0 :: IO (VUM.IOVector Int)
      table_wa <- VUM.replicate (n + 1) 0 :: IO (VUM.IOVector Int)
      (ac, wa) <- simulate pss' table_ac table_wa 0 0
      putStrLn $ (show ac) ++ " " ++ (show wa)
