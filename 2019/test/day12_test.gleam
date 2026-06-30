import day12
import gleam/int
import gleam/result
import lib/aoc

pub fn part1_test() {
  assert aoc.Day12
    |> aoc.read_input
    |> result.try(day12.part1)
    == Ok(int.to_string(day12.part1_answer))
}

pub fn part2_test() {
  assert aoc.Day12
    |> aoc.read_input
    |> result.try(day12.part2)
    == Ok(int.to_string(day12.part2_answer))
}
