import Data.Set qualified as Set

start :: (Int, Int)
start = (0, 0)

step :: (Num b, Num a) => Char -> (a, b) -> (a, b)
step '^' (x, y) = (x, y + 1)
step '>' (x, y) = (x + 1, y)
step 'v' (x, y) = (x, y - 1)
step '<' (x, y) = (x - 1, y)

deliver :: String -> Int
deliver = go start (Set.singleton start)
  where
    go pos visited "" = Set.size visited
    go pos visited (d : directions) = go next (Set.insert next visited) directions
      where
        next = step d pos

main :: IO ()
main = do
  input <- getContents

  let part1 = deliver input
  let part2 = "TODO"

  putStrLn ("Part 1: " ++ show part1)
  putStrLn ("Part 2: " ++ show part2)