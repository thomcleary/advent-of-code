import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import simplifile

const input_filepath = "./inputs/day_02.txt"

pub fn main() -> Nil {
  let assert Ok(input) = simplifile.read(from: input_filepath)
  let assert Ok(program) = program_read(input)

  let assert Ok(part1_answer) = part1(program)
  io.println("Part 1: " <> int.to_string(part1_answer))
}

pub type ArithmeticOp {
  Add
  Multiply
}

type Instruction {
  Arithmetic(op: ArithmeticOp, from_a: Int, from_b: Int, to: Int)
  Halt
}

pub type Program {
  Program(memory: dict.Dict(Int, Int))
}

pub fn program_read(input: String) -> Result(Program, Nil) {
  use integers <- result.try(
    input
    |> string.split(on: ",")
    |> list.index_map(fn(str, index) { #(index, str) })
    |> list.try_map(fn(pair) {
      let #(index, str) = pair
      case int.parse(str) {
        Ok(value) -> Ok(#(index, value))
        Error(_) -> Error(Nil)
      }
    }),
  )

  Ok(Program(memory: dict.from_list(integers)))
}

pub fn part1(program: Program) -> Result(Int, Nil) {
  let alarm_state_memory =
    program.memory
    |> dict.insert(for: 1, insert: 12)
    |> dict.insert(for: 2, insert: 2)

  let altered_program = Program(memory: alarm_state_memory)

  use halted_program <- result.try(program_run(altered_program))

  dict.get(halted_program.memory, 0)
}

pub fn program_run(program: Program) -> Result(Program, Nil) {
  run(program:, instruction_pointer: 0)
}

fn run(
  program program: Program,
  instruction_pointer instruction_pointer: Int,
) -> Result(Program, Nil) {
  use op_code <- result.try(dict.get(program.memory, instruction_pointer))

  use instruction <- result.try(case op_code {
    1 -> to_arithmetic(Add, program:, instruction_pointer:)
    2 -> to_arithmetic(Multiply, program:, instruction_pointer:)
    99 -> Ok(Halt)
    _ -> Error(Nil)
  })

  case instruction {
    Arithmetic(op, from_a, from_b, to) -> {
      use a <- result.try(dict.get(program.memory, from_a))
      use b <- result.try(dict.get(program.memory, from_b))

      let value = case op {
        Add -> a + b
        Multiply -> a * b
      }

      run(
        Program(memory: dict.insert(
          into: program.memory,
          for: to,
          insert: value,
        )),
        next_instruction_pointer(instruction_pointer),
      )
    }

    Halt -> Ok(program)
  }
}

fn to_arithmetic(
  op: ArithmeticOp,
  program program: Program,
  instruction_pointer ip: Int,
) -> Result(Instruction, Nil) {
  use from_a <- result.try(dict.get(program.memory, ip + 1))
  use from_b <- result.try(dict.get(program.memory, ip + 2))
  use to <- result.try(dict.get(program.memory, ip + 3))

  Ok(Arithmetic(op:, from_a:, from_b:, to:))
}

fn next_instruction_pointer(ip: Int) {
  ip + 4
}
