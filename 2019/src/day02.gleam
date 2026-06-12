import gleam/int
import gleam/list
import gleam/result
import lib/intcode

pub const part1_answer = 4_023_471

pub const part2_answer = 8051

pub fn part1(input: String) -> Result(Int, String) {
  use program <- result.try(
    input
    |> intcode.parse_program
    |> result.map_error(intcode.error_to_string),
  )

  run(program, with: Input(noun: 12, verb: 2))
}

pub fn part2(input: String) -> Result(Int, String) {
  use program <- result.try(
    input
    |> intcode.parse_program
    |> result.map_error(intcode.error_to_string),
  )

  let input_range = int.range(from: 0, to: 99 + 1, with: [], run: list.prepend)

  {
    use noun <- list.find_map(input_range)
    use verb <- list.find_map(input_range)

    let program_input = Input(noun:, verb:)

    case run(program, with: program_input) {
      Ok(19_690_720) -> Ok(Ok(program_input))
      Ok(_) -> Error("Try the next input...")
      Error(error) -> Ok(Error(error))
    }
  }
  |> result.replace_error(
    "Failed to find an input that produced the expected output",
  )
  |> result.flatten
  |> result.map(fn(input) { 100 * input.noun + input.verb })
}

type Input {
  Input(noun: Int, verb: Int)
}

fn run(program: intcode.Program, with input: Input) -> Result(Int, String) {
  program
  |> intcode.boot
  |> intcode.poke_memory(at: intcode.Address(1), with: input.noun)
  |> intcode.poke_memory(at: intcode.Address(2), with: input.verb)
  |> intcode.run
  |> result.try(intcode.peek_memory(_, at: intcode.Address(0)))
  |> result.map_error(intcode.error_to_string)
}
