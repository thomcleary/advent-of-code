type Dimensions = (Int, Int, Int)

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn delim xs = case break (== delim) xs of
  (prefix, []) -> [prefix]
  (prefix, _ : rest) -> prefix : splitOn delim rest

getDimensions :: String -> [Dimensions]
getDimensions input = map (parse . splitOn 'x') (lines input)
  where
    parse [l, w, h] = (read l, read w, read h)

wrap :: Dimensions -> Int
wrap (l, w, h) = sum (map (* 2) sides) + minimum sides
  where
    sides = [l * w, w * h, h * l]

part1 :: [Dimensions] -> Int
part1 = sum . map wrap

part2 :: String -> Int
part2 = const (-2)

main :: IO ()
main = do
  input <- getContents

  let dimensions = getDimensions input

  let p1 = part1 dimensions
  -- let p2 = part2 input

  putStrLn ("Part 1: " ++ show p1)
  putStrLn ("Part 2: " ++ "TODO")