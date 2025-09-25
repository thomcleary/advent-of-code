import Data.Set (Set)
import qualified Data.Set as Set

start :: (Int, Int)
start = (0, 0)

step :: (Num b, Num a) => Char -> (a, b) -> (a, b)
step '^' (x, y) = (x, y + 1)
step '>' (x, y) = (x + 1, y)
step 'v' (x, y) = (x, y - 1)
step _ (x, y) = (x - 1, y)

deliver :: String -> Set (Int, Int)
deliver = go start (Set.singleton start)
  where
    go _ visited "" = visited
    go pos visited (d : directions) = go next (Set.insert next visited) directions
      where
        next = step d pos

splitDirections :: String -> (String, String)
splitDirections [] = ([], [])
splitDirections [x] = ([x], [])
splitDirections (x : y : rest) = (x : xs, y : ys)
  where
    (xs, ys) = splitDirections rest

main :: IO ()
main = do
  input <- getContents

  let part1 = Set.size $ deliver input

  let (santaDirs, robotDirs) = splitDirections input
  let part2 = Set.size $ Set.union (deliver santaDirs) (deliver robotDirs)

  putStrLn ("Part 1: " ++ show part1)
  putStrLn ("Part 2: " ++ show part2)