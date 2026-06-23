import day08
import gleam/int
import gleam/result
import lib/aoc

pub fn part1_test() {
  assert aoc.Day08
    |> aoc.read_input
    |> result.try(day08.part1)
    == Ok(int.to_string(day08.part1_answer))
}
