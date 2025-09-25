part1 :: String -> Int
part1 = sum . map (\d -> if d == '(' then 1 else -1)

main :: IO ()
main = do
  input <- getContents
  putStrLn ("Part 1: " ++ show (part1 input))
  putStrLn ("Part 2: " ++ "TODO")