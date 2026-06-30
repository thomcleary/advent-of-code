import day11
import gleam/int
import gleam/result
import lib/aoc

pub fn part1_test() {
  assert aoc.Day11
    |> aoc.read_input
    |> result.try(day11.part1)
    == Ok(int.to_string(day11.part1_answer))
}
