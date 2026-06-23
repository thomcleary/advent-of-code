import day08
import gleam/result
import lib/aoc

pub fn part1_test() {
  assert aoc.Day08
    |> aoc.read_input
    |> result.try(day08.part1)
    == Ok(day08.part1_answer)
}

pub fn part2_test() {
  assert aoc.Day07
    |> aoc.read_input
    |> result.try(day08.part2)
    == Ok(todo)
}
