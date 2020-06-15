import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as VUM

readInt = fst . fromJust . BS.readInt
readString = map BS.unpack . BS.words

getInt = readInt <$> BS.getLine
getNString n = map readString <$> replicateM n BS.getLine

compareRestaurant :: ((String, Int), Int) -> ((String, Int), Int) -> Ordering
compareRestaurant r1 r2 =
  if fst (fst r1) > fst (fst r2)
    then GT
    else
      if fst (fst r1) < fst (fst r2)
        then LT
        else
          if snd (fst r1) > snd (fst r2)
            then LT
            else
              if snd (fst r1) < snd (fst r2)
                then GT
                else EQ

main = do
  n <- getInt
  sps <- getNString n
  let sps' = map (\x -> (head x, read (last x) :: Int)) sps
  let sqs'' = zip sps' [1 .. n]
  let sqs''' = sortBy (compareRestaurant) sqs''
  mapM_ print (map (\x -> snd x) sqs''')
