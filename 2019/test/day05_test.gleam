import day05
import gleam/int
import gleam/result
import lib/aoc

pub fn part1_test() {
  assert aoc.Day05
    |> aoc.read_input
    |> result.try(day05.part1)
    == Ok(int.to_string(day05.part1_answer))
}

pub fn part2_test() {
  assert aoc.Day05
    |> aoc.read_input
    |> result.try(day05.part2)
    == Ok(int.to_string(day05.part2_answer))
}
