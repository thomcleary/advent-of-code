import day04
import gleam/int
import gleam/result
import lib/aoc

fn range(password: String) -> String {
  password <> "-" <> password
}

pub fn part1_example1_test() {
  assert day04.part1(range("111111")) == Ok(int.to_string(1))
}

pub fn part1_example2_test() {
  assert day04.part1(range("223450")) == Ok(int.to_string(0))
}

pub fn part1_example3_test() {
  assert day04.part1(range("123789")) == Ok(int.to_string(0))
}

pub fn part1_test() {
  assert aoc.Day04
    |> aoc.read_input
    |> result.try(day04.part1)
    == Ok(int.to_string(day04.part1_answer))
}

pub fn part2_example1_test() {
  assert day04.part2(range("112233")) == Ok(int.to_string(1))
}

pub fn part2_example2_test() {
  assert day04.part2(range("123444")) == Ok(int.to_string(0))
}

pub fn part2_example3_test() {
  assert day04.part2(range("111122")) == Ok(int.to_string(1))
}

pub fn part2_test() {
  assert aoc.Day04
    |> aoc.read_input
    |> result.try(day04.part2)
    == Ok(int.to_string(day04.part2_answer))
}
