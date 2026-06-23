import gleam/int
import gleam/list
import gleam/result
import gleam/string
import lib/intcode

pub const part1_answer = 15_259_545

pub const part2_answer = 7_616_021

pub fn part1(input: String) -> Result(String, String) {
  use diagnostic_program <- result.try(
    input
    |> intcode.parse_program
    |> result.map_error(intcode.error_to_string),
  )

  use output <- result.try(
    diagnostic_program
    |> run(with_system_id: 1)
    |> result.map(intcode.output),
  )

  case output {
    [diagnostic_code, ..test_outputs] ->
      case int.sum(test_outputs) {
        0 -> Ok(diagnostic_code |> int.to_string)
        _ -> Error(diagnostic_test_error(output))
      }
    [] -> Error(diagnostic_test_error(output))
  }
}

pub fn part2(input: String) -> Result(String, String) {
  use diagnostic_program <- result.try(
    input
    |> intcode.parse_program
    |> result.map_error(intcode.error_to_string),
  )

  use output <- result.try(
    diagnostic_program
    |> run(with_system_id: 5)
    |> result.map(intcode.output),
  )

  output
  |> list.first
  |> result.replace_error(diagnostic_test_error(output))
  |> result.map(int.to_string)
}

fn run(
  program: intcode.Program,
  with_system_id system_id: Int,
) -> Result(intcode.Computer, String) {
  program
  |> intcode.boot
  |> intcode.with_input([system_id])
  |> intcode.run
  |> result.map_error(intcode.error_to_string)
}

fn diagnostic_test_error(outputs: List(Int)) -> String {
  "Diagnostic test failed, test_outputs=["
  <> string.join(list.reverse(outputs) |> list.map(int.to_string), with: ",")
  <> "]"
}
