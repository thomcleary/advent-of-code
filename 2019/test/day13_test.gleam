import day13
import gleam/int
import gleam/result
import lib/aoc

pub fn part1_test() {
  assert aoc.Day13
    |> aoc.read_input
    |> result.try(day13.part1)
    == Ok(int.to_string(day13.part1_answer))
}

pub fn part2_test() {
  assert aoc.Day13
    |> aoc.read_input
    |> result.try(day13.part2)
    == Ok(int.to_string(todo))
}
