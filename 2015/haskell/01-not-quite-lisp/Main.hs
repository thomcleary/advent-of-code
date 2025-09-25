toLevel :: Char -> Int
toLevel '(' = 1
toLevel _ = negate 1

findBasement :: [Int] -> Int
findBasement = go 0 0
  where
    go i (-1) _ = i
    go _ _ [] = error "Basement not found"
    go i level (d : ds) = go (i + 1) (level + d) ds

main :: IO ()
main = do
  input <- getContents

  let levels = map toLevel input

  let part1 = sum levels
  let part2 = findBasement levels

  putStrLn ("Part 1: " ++ show part1)
  putStrLn ("Part 2: " ++ show part2)