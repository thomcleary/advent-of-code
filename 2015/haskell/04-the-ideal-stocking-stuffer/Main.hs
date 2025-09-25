import Data.Digest.Pure.MD5 (md5)
import Data.List (isPrefixOf)
import qualified Data.String as ByteString

mine :: String -> Int -> Int
mine key zeros = go 1
  where
    isMatch = isPrefixOf $ replicate zeros '0'
    go n
      | isMatch $ show (md5 (ByteString.fromString (key ++ show n))) = n
      | otherwise = go (n + 1)

main :: IO ()
main = do
  key <- getContents

  let part1 = mine key 5
  let part2 = mine key 6

  putStrLn ("Part 1: " ++ show part1)
  putStrLn ("Part 2: " ++ show part2)