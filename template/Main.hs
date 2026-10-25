{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- AtCoder 提出用テンプレート (Haskell GHC 9.8.4 / GHC2021)
-- 既存解答で繰り返し使っていた ByteString ベースの入力ヘルパーをまとめたもの。
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)

-- ---------------------------------------------------------------- 入力
readInt :: BS.ByteString -> Int
readInt = fst . fromJust . BS.readInt

readInteger :: BS.ByteString -> Integer
readInteger = fst . fromJust . BS.readInteger

readIntList :: BS.ByteString -> [Int]
readIntList = map readInt . BS.words

readString :: BS.ByteString -> [String]
readString = map BS.unpack . BS.words

-- | 1 行に整数 1 つ
getInt :: IO Int
getInt = readInt <$> BS.getLine

-- | 1 行に整数 1 つ (多倍長)
getInteger :: IO Integer
getInteger = readInteger <$> BS.getLine

-- | 1 行に空白区切りの整数
getIntList :: IO [Int]
getIntList = readIntList <$> BS.getLine

-- | 1 行に空白区切りの整数 2 つ
getInt2 :: IO (Int, Int)
getInt2 = do
  [a, b] <- getIntList
  return (a, b)

-- | 1 行に空白区切りの整数 3 つ
getInt3 :: IO (Int, Int, Int)
getInt3 = do
  [a, b, c] <- getIntList
  return (a, b, c)

-- | 1 行に空白区切りの文字列
getStringList :: IO [String]
getStringList = readString <$> BS.getLine

-- | 1 行をそのまま文字列として
getString :: IO String
getString = BS.unpack <$> BS.getLine

-- | n 行それぞれに整数 1 つ
getNInt :: Int -> IO [Int]
getNInt n = replicateM n getInt

-- | n 行それぞれに空白区切りの整数
getNIntList :: Int -> IO [[Int]]
getNIntList n = replicateM n getIntList

-- ---------------------------------------------------------------- 出力
-- | 空白区切りで 1 行に出力
printList :: Show a => [a] -> IO ()
printList = putStrLn . unwords . map show

yesNo :: Bool -> String
yesNo b = if b then "Yes" else "No"

-- ---------------------------------------------------------------- 解答
main :: IO ()
main = do
  n <- getInt
  xs <- getIntList
  print $ n + sum xs
