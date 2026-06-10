import gleam/int
import gleam/list
import gleam/result
import lib/intcode
import lib/part

pub fn solve(input: String) -> Nil {
  let assert Ok(program) = intcode.parse_program(input)

  program
  |> part1
  |> part.try_print(part.One)

  program
  |> part2
  |> part.try_print(part.Two)
}

fn part1(program: intcode.Program) -> Result(Int, Nil) {
  program
  |> run(with: Input(noun: 12, verb: 2))
}

fn part2(program: intcode.Program) -> Result(Int, Nil) {
  use input <- result.map(
    find_input(with: fn(input) {
      program
      |> run(with: input)
      |> fn(output) {
        case output {
          Ok(19_690_720) -> Ok(input)
          _ -> Error(Nil)
        }
      }
    }),
  )

  100 * input.noun + input.verb
}

type Input {
  Input(noun: Int, verb: Int)
}

fn run(program: intcode.Program, with input: Input) -> Result(Int, Nil) {
  program
  |> intcode.boot
  |> intcode.poke_memory(at: intcode.Address(1), with: input.noun)
  |> intcode.poke_memory(at: intcode.Address(2), with: input.verb)
  |> intcode.run
  |> result.try(intcode.peek_memory(_, at: intcode.Address(0)))
}

fn find_input(
  with with: fn(Input) -> Result(Input, Nil),
) -> Result(Input, Nil) {
  let input_range = int.range(from: 0, to: 99 + 1, with: [], run: list.prepend)

  list.find_map(input_range, fn(noun) {
    list.find_map(input_range, fn(verb) { with(Input(noun:, verb:)) })
  })
}
