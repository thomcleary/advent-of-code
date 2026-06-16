import day02
import gleam/result
import lib/aoc

pub fn part1_test() {
  assert aoc.Day02
    |> aoc.read_input
    |> result.try(day02.part1)
    == Ok(day02.part1_answer)
}

pub fn part2_test() {
  assert aoc.Day02
    |> aoc.read_input
    |> result.try(day02.part2)
    == Ok(day02.part2_answer)
}
