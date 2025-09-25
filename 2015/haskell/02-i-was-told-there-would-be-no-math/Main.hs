type Dimensions = (Int, Int, Int)

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn delim xs = case break (== delim) xs of
  (prefix, []) -> [prefix]
  (prefix, _ : rest) -> prefix : splitOn delim rest

getDimensions :: String -> [Dimensions]
getDimensions input = map (parse . splitOn 'x') (lines input)
  where
    parse [l, w, h] = (read l, read w, read h)

wrappingPaper :: Dimensions -> Int
wrappingPaper (l, w, h) = sum (map (* 2) sides) + minimum sides
  where
    sides = [l * w, w * h, h * l]

ribbon :: Dimensions -> Int
ribbon (l, w, h) = (l * w * h) + (2 * minimum [l + w, l + h, w + h])

main :: IO ()
main = do
  input <- getContents

  let dimensions = getDimensions input

  let part1 = sum . map wrappingPaper $ dimensions
  let part2 = sum . map ribbon $ dimensions

  putStrLn ("Part 1: " ++ show part1)
  putStrLn ("Part 2: " ++ show part2)