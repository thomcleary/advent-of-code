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

pub fn day5_part2_position_mode_equals_test() {
  let assert Ok(program) = intcode.parse_program("3,9,8,9,10,9,4,9,99,-1,8")

  assert program
    |> intcode.boot
    |> intcode.with_input([8])
    |> intcode.run
    |> result.map(intcode.output)
    == Ok([1])

  assert program
    |> intcode.boot
    |> intcode.with_input([9])
    |> intcode.run
    |> result.map(intcode.output)
    == Ok([0])
}

pub fn day5_part2_position_mode_less_than_test() {
  let assert Ok(program) = intcode.parse_program("3,9,7,9,10,9,4,9,99,-1,8")

  assert program
    |> intcode.boot
    |> intcode.with_input([7])
    |> intcode.run
    |> result.map(intcode.output)
    == Ok([1])

  assert program
    |> intcode.boot
    |> intcode.with_input([8])
    |> intcode.run
    |> result.map(intcode.output)
    == Ok([0])

  assert program
    |> intcode.boot
    |> intcode.with_input([9])
    |> intcode.run
    |> result.map(intcode.output)
    == Ok([0])
}

pub fn day5_part2_immediate_mode_equals_test() {
  let assert Ok(program) = intcode.parse_program("3,3,1108,-1,8,3,4,3,99")

  assert program
    |> intcode.boot
    |> intcode.with_input([8])
    |> intcode.run
    |> result.map(intcode.output)
    == Ok([1])

  assert program
    |> intcode.boot
    |> intcode.with_input([9])
    |> intcode.run
    |> result.map(intcode.output)
    == Ok([0])
}

pub fn day5_part2_immediate_mode_less_than_test() {
  let assert Ok(program) = intcode.parse_program("3,3,1107,-1,8,3,4,3,99")

  assert program
    |> intcode.boot
    |> intcode.with_input([7])
    |> intcode.run
    |> result.map(intcode.output)
    == Ok([1])

  assert program
    |> intcode.boot
    |> intcode.with_input([8])
    |> intcode.run
    |> result.map(intcode.output)
    == Ok([0])

  assert program
    |> intcode.boot
    |> intcode.with_input([9])
    |> intcode.run
    |> result.map(intcode.output)
    == Ok([0])
}

pub fn day5_part2_position_mode_jump_test() {
  let assert Ok(program) =
    intcode.parse_program("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9")

  assert program
    |> intcode.boot
    |> intcode.with_input([0])
    |> intcode.run
    |> result.map(intcode.output)
    == Ok([0])

  assert program
    |> intcode.boot
    |> intcode.with_input([1])
    |> intcode.run
    |> result.map(intcode.output)
    == Ok([1])

  assert program
    |> intcode.boot
    |> intcode.with_input([-2])
    |> intcode.run
    |> result.map(intcode.output)
    == Ok([1])
}

pub fn day5_part2_immediate_mode_jump_test() {
  let assert Ok(program) =
    intcode.parse_program("3,3,1105,-1,9,1101,0,0,12,4,12,99,1")

  assert program
    |> intcode.boot
    |> intcode.with_input([0])
    |> intcode.run
    |> result.map(intcode.output)
    == Ok([0])

  assert program
    |> intcode.boot
    |> intcode.with_input([1])
    |> intcode.run
    |> result.map(intcode.output)
    == Ok([1])

  assert program
    |> intcode.boot
    |> intcode.with_input([-2])
    |> intcode.run
    |> result.map(intcode.output)
    == Ok([1])
}

pub fn day5_part2_larger_example_test() {
  let assert Ok(program) =
    intcode.parse_program(
      "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99",
    )

  assert program
    |> intcode.boot
    |> intcode.with_input([7])
    |> intcode.run
    |> result.map(intcode.output)
    == Ok([999])

  assert program
    |> intcode.boot
    |> intcode.with_input([8])
    |> intcode.run
    |> result.map(intcode.output)
    == Ok([1000])

  assert program
    |> intcode.boot
    |> intcode.with_input([9])
    |> intcode.run
    |> result.map(intcode.output)
    == Ok([1001])
}
