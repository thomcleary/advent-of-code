import gleam/int
import gleam/io
import gleam/list
import gleam/result
import intcode
import simplifile

const input_filepath = "./inputs/day_02.txt"

pub fn main() -> Nil {
  let assert Ok(input) = simplifile.read(from: input_filepath)
  let assert Ok(program) = intcode.parse_program(input)

  let assert Ok(part1_answer) = part1(program)
  io.println("Part 1: " <> int.to_string(part1_answer))

  let assert Ok(part2_answer) = part2(program)
  io.println("Part 2: " <> int.to_string(part2_answer))
}

fn part1(program: intcode.Program) -> Result(Int, Nil) {
  use halted <- result.try(
    program
    |> intcode.load_program
    |> intcode.poke_memory(intcode.Address(1), 12)
    |> intcode.poke_memory(intcode.Address(2), 2)
    |> intcode.run_program,
  )

  halted
  |> intcode.peek_memory(intcode.Address(0))
}

fn part2(program: intcode.Program) -> Result(Int, Nil) {
  let input_range = int.range(from: 0, to: 100, with: [], run: list.prepend)

  list.find_map(input_range, fn(noun) {
    list.find_map(input_range, fn(verb) {
      use halted <- result.try(
        program
        |> intcode.load_program
        |> intcode.poke_memory(intcode.Address(1), noun)
        |> intcode.poke_memory(intcode.Address(2), verb)
        |> intcode.run_program,
      )

      case intcode.peek_memory(halted, intcode.Address(0)) {
        Ok(19_690_720) -> Ok(100 * noun + verb)
        _ -> Error(Nil)
      }
    })
  })
}
