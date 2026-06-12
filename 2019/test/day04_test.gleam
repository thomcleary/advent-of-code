import day04
import lib/aoc

fn range(password: String) -> String {
  password <> "-" <> password
}

pub fn part1_example1_test() {
  assert day04.part1(range("111111")) == Ok(1)
}

pub fn part1_example2_test() {
  assert day04.part1(range("223450")) == Ok(0)
}

pub fn part1_example3_test() {
  assert day04.part1(range("123789")) == Ok(0)
}

pub fn part1_test() {
  let assert Ok(input) = aoc.read_input(aoc.Day04)

  assert day04.part1(input) == Ok(day04.part1_answer)
}

pub fn part2_example1_test() {
  assert day04.part2(range("112233")) == Ok(1)
}

pub fn part2_example2_test() {
  assert day04.part2(range("123444")) == Ok(0)
}

pub fn part2_example3_test() {
  assert day04.part2(range("111122")) == Ok(1)
}

pub fn part2_test() {
  let assert Ok(input) = aoc.read_input(aoc.Day04)

  assert day04.part2(input) == Ok(day04.part2_answer)
}
