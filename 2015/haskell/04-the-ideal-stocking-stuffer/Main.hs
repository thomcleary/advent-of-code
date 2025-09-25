import Data.Digest.Pure.MD5 (md5)
import Data.List (isPrefixOf)
import qualified Data.String as ByteString

mine :: String -> Int
mine key = go 1
  where
    go n
      | "00000" `isPrefixOf` show (md5 (ByteString.fromString (key ++ show n))) = n
      | otherwise = go (n + 1)

main :: IO ()
main = do
  key <- getContents

  let part1 = mine key
  let part2 = "TODO"

  putStrLn ("Part 1: " ++ show part1)
  putStrLn ("Part 2: " ++ show part2)