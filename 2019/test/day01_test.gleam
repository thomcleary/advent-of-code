import day01
import gleam/result
import lib/aoc

pub fn part1_example1_test() {
  assert day01.part1("12") == Ok(2)
}

pub fn part1_example2_test() {
  assert day01.part1("14") == Ok(2)
}

pub fn part1_example3_test() {
  assert day01.part1("1969") == Ok(654)
}

pub fn part1_example4_test() {
  assert day01.part1("100756") == Ok(33_583)
}

pub fn part1_test() {
  assert aoc.Day01
    |> aoc.read_input
    |> result.try(day01.part1)
    == Ok(day01.part1_answer)
}

pub fn part2_example1_test() {
  assert day01.part2("14") == Ok(2)
}

pub fn part2_example2_test() {
  assert day01.part2("1969") == Ok(966)
}

pub fn part2_example3_test() {
  assert day01.part2("100756") == Ok(50_346)
}

pub fn part2_test() {
  assert aoc.Day01
    |> aoc.read_input
    |> result.try(day01.part2)
    == Ok(day01.part2_answer)
}
