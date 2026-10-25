import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

readInt = fst . fromJust . BS.readInt

getInt = readInt <$> BS.getLine

main = do
  n <- getInt
  mapM_ print (n : (replicate n 1))
