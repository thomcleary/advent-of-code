import day09
import gleam/int
import gleam/result
import lib/aoc

pub fn part1_test() {
  assert aoc.Day09
    |> aoc.read_input
    |> result.try(day09.part1)
    == Ok(int.to_string(day09.part1_answer))
}

pub fn part2_test() {
  assert aoc.Day09
    |> aoc.read_input
    |> result.try(day09.part2)
    == Ok(todo)
}
