toLevel :: Char -> Int
toLevel '(' = 1
toLevel ')' = negate 1

findBasement :: [Int] -> Int
findBasement = go 0 0
  where
    go i (-1) ds = i
    go i floor (d : ds) = go (i + 1) (floor + d) ds

main :: IO ()
main = do
  input <- getContents

  let levels = map toLevel input

  let part1 = sum levels
  let part2 = findBasement levels

  putStrLn ("Part 1: " ++ show part1)
  putStrLn ("Part 2: " ++ show part2)