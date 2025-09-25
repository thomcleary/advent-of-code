toLevel :: Char -> Int
toLevel '(' = 1
toLevel ')' = negate 1

part1 :: String -> Int
part1 = sum . map toLevel

part2 :: String -> Int
part2 directions = go 0 0 (map toLevel directions)
  where
    go i (-1) ds = i
    go i floor (d : ds) = go (i + 1) (floor + d) ds

main :: IO ()
main = do
  input <- getContents

  let p1 = part1 input
  let p2 = part2 input

  putStrLn ("Part 1: " ++ show p1)
  putStrLn ("Part 2: " ++ show p2)