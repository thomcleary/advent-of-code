import day_02
import gleam/dict
import gleam/int
import gleam/list
import gleam/string

pub fn part1_example_test() {
  run_test(
    program: "1,9,10,3,2,3,11,0,99,30,40,50",
    expected: "3500,9,10,70,2,3,11,0,99,30,40,50",
  )
}

pub fn part1_add_test() {
  run_test(program: "1,0,0,0,99", expected: "2,0,0,0,99")
}

pub fn part1_multiply_test() {
  run_test(program: "2,3,0,3,99", expected: "2,3,0,6,99")
}

pub fn part1_big_multiply_test() {
  run_test(program: "2,4,4,5,99,0", expected: "2,4,4,5,99,9801")
}

pub fn part1_add_and_multiply_test() {
  run_test(program: "1,1,1,4,99,5,6,0,99", expected: "30,1,1,4,2,5,6,0,99")
}

fn run_test(program program: String, expected expected: String) {
  let assert Ok(program) = day_02.program_read(program)
  let assert Ok(final) = day_02.program_run(program)

  let actual =
    final.memory
    |> dict.values
    |> list.map(int.to_string)
    |> string.join(with: ",")

  assert actual == expected
}
