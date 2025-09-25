import Data.List (isInfixOf)

hasAtleastThreeVowels :: String -> Bool
hasAtleastThreeVowels str = length (filter (`elem` ['a', 'e', 'i', 'o', 'u']) str) >= 3

hasRepeatingCharacter :: String -> Bool
hasRepeatingCharacter (x : y : rest) = x == y || hasRepeatingCharacter (y : rest)
hasRepeatingCharacter _ = False

hasNaughtySubstring :: String -> Bool
hasNaughtySubstring str = any (`isInfixOf` str) ["ab", "cd", "pq", "xy"]

isNice :: String -> Bool
isNice str =
  hasAtleastThreeVowels str
    && hasRepeatingCharacter str
    && not (hasNaughtySubstring str)

main :: IO ()
main = do
  input <- getContents

  let strs = lines input

  let part1 = length $ filter isNice strs
  let part2 = "TODO"

  putStrLn ("Part 1: " ++ show part1)
  putStrLn ("Part 2: " ++ show part2)