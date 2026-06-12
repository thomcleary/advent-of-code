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

pub fn parse_program(input: String) -> Result(Program, String) {
  use integers <- result.map(
    input
    |> string.trim
    |> string.split(on: ",")
    |> list.try_map(int.parse)
    |> result.replace_error("Program contains non integer value"),
  )

  Program(code: integers)
}

pub fn address_offset(address: Address, offset: Int) -> Address {
  let Address(a) = address
  Address(a + offset)
}

pub fn peek_memory(
  computer: Computer,
  at addr: Address,
) -> Result(Int, String) {
  dict.get(computer.memory, addr)
  |> result.replace_error(
    "Failed to get memory from address [" <> int.to_string(addr.value) <> "]",
  )
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
  |> dict.to_list()
  |> list.sort(by: fn(a, b) {
    let #(Address(a_addr), _) = a
    let #(Address(b_addr), _) = b
    int.compare(a_addr, b_addr)
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

pub fn run(computer: Computer) -> Result(Computer, String) {
  use op_code <- result.try(
    computer |> peek_memory(at: computer.instruction_pointer),
  )

  use instruction <- result.try(case op_code {
    1 -> computer |> read_binary_op(with: int.add) |> result.map(Add)
    2 -> computer |> read_binary_op(with: int.multiply) |> result.map(Multiply)
    99 -> Ok(Halt)
    _ -> Error("Invalid op code [" <> int.to_string(op_code) <> "]")
  })

  case instruction {
    Add(op) | Multiply(op) -> computer |> run_binary_op(op) |> result.try(run)
    Halt -> Ok(computer)
  }
}

fn read_binary_op(
  computer: Computer,
  with apply: fn(Int, Int) -> Int,
) -> Result(BinaryOp, String) {
  let from = computer.instruction_pointer

  use a <- result.try(computer |> peek_memory(at: from |> address_offset(1)))
  use b <- result.try(computer |> peek_memory(at: from |> address_offset(2)))
  use dest <- result.map(computer |> peek_memory(at: from |> address_offset(3)))

  BinaryOp(apply:, a: Address(a), b: Address(b), dest: Address(dest))
}

fn run_binary_op(computer: Computer, op: BinaryOp) -> Result(Computer, String) {
  use a <- result.try(computer |> peek_memory(at: op.a))
  use b <- result.map(computer |> peek_memory(at: op.b))

  computer
  |> poke_memory(at: op.dest, with: op.apply(a, b))
  |> ip_advance(by: 4)
}

fn ip_advance(computer: Computer, by offset: Int) -> Computer {
  Computer(
    ..computer,
    instruction_pointer: address_offset(computer.instruction_pointer, offset),
  )
}
