import gleam/list
import gleam/pair
import intcode

pub fn part1_example_test() {
  run_test(
    program: intcode.Program([1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]),
    expected: intcode.Program([3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50]),
  )
}

pub fn part1_add_test() {
  run_test(
    program: intcode.Program([1, 0, 0, 0, 99]),
    expected: intcode.Program([2, 0, 0, 0, 99]),
  )
}

pub fn part1_multiply_test() {
  run_test(
    program: intcode.Program([2, 3, 0, 3, 99]),
    expected: intcode.Program([2, 3, 0, 6, 99]),
  )
}

pub fn part1_big_multiply_test() {
  run_test(
    program: intcode.Program([2, 4, 4, 5, 99, 0]),
    expected: intcode.Program([2, 4, 4, 5, 99, 9801]),
  )
}

pub fn part1_add_and_multiply_test() {
  run_test(
    program: intcode.Program([1, 1, 1, 4, 99, 5, 6, 0, 99]),
    expected: intcode.Program([30, 1, 1, 4, 2, 5, 6, 0, 99]),
  )
}

fn run_test(
  program program: intcode.Program,
  expected expected: intcode.Program,
) {
  let assert Ok(halted) =
    program
    |> intcode.load_program
    |> intcode.run_program

  let actual =
    halted
    |> intcode.dump_memory
    |> list.map(pair.second)

  assert actual == expected.code
}
