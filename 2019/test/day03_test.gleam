import day03
import gleam/int
import gleam/result
import lib/aoc

const example1 = "R8,U5,L5,D3
U7,R6,D4,L4"

const example2 = "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83"

const example3 = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"

pub fn part1_example1_test() {
  assert day03.part1(example1) == Ok(int.to_string(6))
}

pub fn part1_example2_test() {
  assert day03.part1(example2) == Ok(int.to_string(159))
}

pub fn part1_example3_test() {
  assert day03.part1(example3) == Ok(int.to_string(135))
}

pub fn part1_test() {
  assert aoc.Day03
    |> aoc.read_input
    |> result.try(day03.part1)
    == Ok(int.to_string(day03.part1_answer))
}

pub fn part2_example1_test() {
  assert day03.part2(example1) == Ok(int.to_string(30))
}

pub fn part2_example2_test() {
  assert day03.part2(example2) == Ok(int.to_string(610))
}

pub fn part2_example3_test() {
  assert day03.part2(example3) == Ok(int.to_string(410))
}

pub fn part2_test() {
  assert aoc.Day03
    |> aoc.read_input
    |> result.try(day03.part2)
    == Ok(int.to_string(day03.part2_answer))
}
