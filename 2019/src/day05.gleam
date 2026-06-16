import gleam/int
import gleam/list
import gleam/result
import gleam/string
import lib/intcode

pub const part1_answer = 15_259_545

pub fn part1(input: String) -> Result(Int, String) {
  use diagnostic_program <- result.try(
    input
    |> intcode.parse_program
    |> result.map_error(intcode.error_to_string),
  )

  use halted <- result.try(
    diagnostic_program
    |> intcode.boot
    |> intcode.with_input([1])
    |> intcode.run
    |> result.map_error(intcode.error_to_string),
  )

  case intcode.output(halted) {
    [diagnostic_code, ..test_outputs] ->
      case int.sum(test_outputs) {
        0 -> Ok(diagnostic_code)
        _ -> Error(diagnostic_test_error(intcode.output(halted)))
      }
    [] -> Error(diagnostic_test_error(intcode.output(halted)))
  }
}

pub fn part2(_input: String) -> Result(Int, String) {
  Ok(-1)
}

fn diagnostic_test_error(outputs: List(Int)) -> String {
  "Diagnostic test failed, test_outputs=["
  <> string.join(list.reverse(outputs) |> list.map(int.to_string), with: ",")
  <> "]"
}
