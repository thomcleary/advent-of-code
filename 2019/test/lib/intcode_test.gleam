import gleam/int
import gleam/list
import gleam/pair
import gleam/result
import gleam/string
import lib/intcode

fn memory(computer: intcode.Computer) -> String {
  computer
  |> intcode.dump_memory
  |> list.map(pair.second)
  |> list.map(int.to_string)
  |> string.join(with: ",")
}

pub fn day2_part1_example1_test() {
  let assert Ok(program) =
    intcode.parse_program("1,9,10,3,2,3,11,0,99,30,40,50")

  assert program
    |> intcode.boot
    |> intcode.run
    |> result.map(memory)
    == Ok("3500,9,10,70,2,3,11,0,99,30,40,50")
}

pub fn day2_part1_example2_test() {
  let assert Ok(program) = intcode.parse_program("1,0,0,0,99")

  assert program
    |> intcode.boot
    |> intcode.run
    |> result.map(memory)
    == Ok("2,0,0,0,99")
}

pub fn day2_part1_example3_test() {
  let assert Ok(program) = intcode.parse_program("2,3,0,3,99")

  assert program
    |> intcode.boot
    |> intcode.run
    |> result.map(memory)
    == Ok("2,3,0,6,99")
}

pub fn day2_part1_example4_test() {
  let assert Ok(program) = intcode.parse_program("2,4,4,5,99,0")

  assert program
    |> intcode.boot
    |> intcode.run
    |> result.map(memory)
    == Ok("2,4,4,5,99,9801")
}

pub fn day2_part1_example5_test() {
  let assert Ok(program) = intcode.parse_program("1,1,1,4,99,5,6,0,99")

  assert program
    |> intcode.boot
    |> intcode.run
    |> result.map(memory)
    == Ok("30,1,1,4,2,5,6,0,99")
}

pub fn day5_part1_example1_test() {
  let input = [42]

  let assert Ok(program) = intcode.parse_program("3,0,4,0,99")

  assert program
    |> intcode.boot
    |> intcode.with_input(input)
    |> intcode.run
    |> result.map(intcode.output)
    == Ok(input)
}

pub fn day5_part1_example2_test() {
  let assert Ok(program) = intcode.parse_program("1002,4,3,4,33")

  assert program
    |> intcode.boot
    |> intcode.run
    |> result.map(memory)
    == Ok("1002,4,3,4,99")
}

pub fn day5_part1_example3_test() {
  let assert Ok(program) = intcode.parse_program("1101,100,-1,4,0")

  assert program
    |> intcode.boot
    |> intcode.run
    |> result.map(memory)
    == Ok("1101,100,-1,4,99")
}
