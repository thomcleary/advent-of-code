import day06
import gleam/result
import lib/aoc

pub fn part1_example_test() {
  let map =
    "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L"

  assert day06.part1(map) == Ok(42)
}

pub fn part1_test() {
  assert aoc.Day06
    |> aoc.read_input
    |> result.try(day06.part1)
    == Ok(day06.part1_answer)
}

pub fn part2_example_test() {
  let map =
    "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN"

  assert day06.part2(map) == Ok(4)
}

pub fn part2_test() {
  assert aoc.Day06
    |> aoc.read_input
    |> result.try(day06.part2)
    == Ok(day06.part2_answer)
}
