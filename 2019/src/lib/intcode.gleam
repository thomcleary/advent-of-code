import gleam/dict
import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub type Program {
  Program(code: List(Int))
}

pub type Address {
  Address(value: Int)
}

pub opaque type Computer {
  Computer(memory: dict.Dict(Address, Int), instruction_pointer: Address)
}

type BinaryOp {
  BinaryOp(apply: fn(Int, Int) -> Int, a: Address, b: Address, dest: Address)
}

type Instruction {
  Add(BinaryOp)
  Multiply(BinaryOp)
  Halt
}

pub type IntcodeError {
  ParseError
  SegmentationFault(at: Address)
  InvalidOpcode(opcode: Int)
}

pub fn error_to_string(err: IntcodeError) -> String {
  case err {
    ParseError -> "Failed to parse program"
    SegmentationFault(address) ->
      "Segmentation fault at address [" <> int.to_string(address.value) <> "]"
    InvalidOpcode(opcode) -> "Invalid opcode [" <> int.to_string(opcode) <> "]"
  }
}

pub fn parse_program(input: String) -> Result(Program, IntcodeError) {
  use integers <- result.map(
    input
    |> string.trim
    |> string.split(on: ",")
    |> list.try_map(int.parse)
    |> result.replace_error(ParseError),
  )

  Program(code: integers)
}

pub fn address_at_offset(by offset: Int, from address: Address) -> Address {
  Address(address.value + offset)
}

pub fn peek_memory(
  computer: Computer,
  at address: Address,
) -> Result(Int, IntcodeError) {
  dict.get(computer.memory, address)
  |> result.replace_error(SegmentationFault(at: address))
}

pub fn poke_memory(
  computer: Computer,
  at addr: Address,
  with value: Int,
) -> Computer {
  Computer(
    ..computer,
    memory: dict.insert(computer.memory, for: addr, insert: value),
  )
}

pub fn dump_memory(computer: Computer) -> List(#(Address, Int)) {
  computer.memory
  |> dict.to_list
  |> list.sort(by: fn(a, b) {
    let #(a_addr, _) = a
    let #(b_addr, _) = b
    int.compare(a_addr.value, b_addr.value)
  })
}

pub fn boot(program: Program) -> Computer {
  Computer(
    instruction_pointer: Address(0),
    memory: program.code
      |> list.index_map(fn(integer, index) { #(Address(index), integer) })
      |> dict.from_list,
  )
}

pub fn run(computer: Computer) -> Result(Computer, IntcodeError) {
  use op_code <- result.try(
    computer |> peek_memory(at: computer.instruction_pointer),
  )

  use instruction <- result.try(case op_code {
    1 -> computer |> read_binary_op(with: int.add) |> result.map(Add)
    2 -> computer |> read_binary_op(with: int.multiply) |> result.map(Multiply)
    99 -> Ok(Halt)
    code -> Error(InvalidOpcode(code))
  })

  case instruction {
    Add(op) | Multiply(op) -> computer |> run_binary_op(op) |> result.try(run)
    Halt -> Ok(computer)
  }
}

fn read_binary_op(
  computer: Computer,
  with apply: fn(Int, Int) -> Int,
) -> Result(BinaryOp, IntcodeError) {
  let offset_from_ip_by = address_at_offset(
    by: _,
    from: computer.instruction_pointer,
  )

  use a <- result.try(computer |> peek_memory(at: offset_from_ip_by(1)))
  use b <- result.try(computer |> peek_memory(at: offset_from_ip_by(2)))
  use dest <- result.map(computer |> peek_memory(at: offset_from_ip_by(3)))

  BinaryOp(apply:, a: Address(a), b: Address(b), dest: Address(dest))
}

fn run_binary_op(
  computer: Computer,
  op: BinaryOp,
) -> Result(Computer, IntcodeError) {
  use a <- result.try(computer |> peek_memory(at: op.a))
  use b <- result.map(computer |> peek_memory(at: op.b))

  computer
  |> poke_memory(at: op.dest, with: op.apply(a, b))
  |> advance_instruction_pointer(by: 4)
}

fn advance_instruction_pointer(computer: Computer, by offset: Int) -> Computer {
  Computer(
    ..computer,
    instruction_pointer: address_at_offset(
      offset,
      from: computer.instruction_pointer,
    ),
  )
}
