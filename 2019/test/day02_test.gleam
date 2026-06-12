import day02
import lib/aoc

pub fn part1_test() {
  let assert Ok(input) = aoc.read_input(aoc.Day02)

  assert day02.part1(input) == Ok(day02.part1_answer)
}

pub fn part2_test() {
  let assert Ok(input) = aoc.read_input(aoc.Day02)

  assert day02.part2(input) == Ok(day02.part2_answer)
}
