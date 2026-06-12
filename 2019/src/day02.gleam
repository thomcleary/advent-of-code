import gleam/int
import gleam/list
import gleam/result
import lib/intcode

pub const part1_answer = 4_023_471

pub const part2_answer = 8051

pub fn part1(input: String) -> Result(Int, String) {
  use program <- result.try(intcode.parse_program(input))

  run(program, with: Input(noun: 12, verb: 2))
}

pub fn part2(input: String) -> Result(Int, String) {
  use program <- result.try(intcode.parse_program(input))

  use program_input <- result.map(
    find_input(with: fn(input) {
      program
      |> run(with: input)
      |> fn(output) {
        case output {
          Ok(19_690_720) -> Ok(Ok(input))
          Ok(_) -> Error("Not what we're looking for")
          Error(reason) -> Ok(Error(reason))
        }
      }
    }),
  )

  100 * program_input.noun + program_input.verb
}

type Input {
  Input(noun: Int, verb: Int)
}

fn find_input(
  with with: fn(Input) -> Result(Result(Input, String), String),
) -> Result(Input, String) {
  let input_range = int.range(from: 0, to: 99 + 1, with: [], run: list.prepend)

  list.find_map(input_range, fn(noun) {
    list.find_map(input_range, fn(verb) { with(Input(noun:, verb:)) })
  })
  |> result.replace_error(
    "Failed to find an input that produced the expected output",
  )
  |> result.flatten
}

fn run(program: intcode.Program, with input: Input) -> Result(Int, String) {
  program
  |> intcode.boot
  |> intcode.poke_memory(at: intcode.Address(1), with: input.noun)
  |> intcode.poke_memory(at: intcode.Address(2), with: input.verb)
  |> intcode.run
  |> result.try(intcode.peek_memory(_, at: intcode.Address(0)))
}
