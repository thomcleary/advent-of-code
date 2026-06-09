import gleam/dict
import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub type Program {
  Program(code: List(Int))
}

pub type Address {
  Address(Int)
}

pub opaque type Computer {
  Computer(memory: dict.Dict(Address, Int), instruction_pointer: Address)
}

type BinaryOp {
  BinaryOp(
    apply: fn(Int, Int) -> Int,
    a_addr: Address,
    b_addr: Address,
    dest_addr: Address,
  )
}

type Instruction {
  Add(BinaryOp)
  Multiply(BinaryOp)
  Halt
}

pub fn parse_program(input: String) -> Result(Program, Nil) {
  use integers <- result.try(
    input
    |> string.trim
    |> string.split(on: ",")
    |> list.try_map(int.parse),
  )

  Ok(Program(code: integers))
}

pub fn address_offset(address: Address, offset: Int) -> Address {
  let Address(a) = address
  Address(a + offset)
}

pub fn peek_memory(computer: Computer, addr addr: Address) -> Result(Int, Nil) {
  computer.memory |> dict.get(addr)
}

pub fn poke_memory(
  computer: Computer,
  addr addr: Address,
  value value: Int,
) -> Computer {
  Computer(
    ..computer,
    memory: computer.memory |> dict.insert(for: addr, insert: value),
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

pub fn load_program(program: Program) -> Computer {
  Computer(
    instruction_pointer: Address(0),
    memory: program.code
      |> list.index_map(fn(integer, index) { #(Address(index), integer) })
      |> dict.from_list,
  )
}

pub fn run_program(computer computer: Computer) -> Result(Computer, Nil) {
  use op_code <- result.try(
    computer |> peek_memory(computer.instruction_pointer),
  )

  use instruction <- result.try(case op_code {
    1 -> computer |> binary_op_add |> result.map(Add)
    2 -> computer |> binary_op_multiply |> result.map(Multiply)
    99 -> Ok(Halt)
    _ -> Error(Nil)
  })

  case instruction {
    Add(binary_op) | Multiply(binary_op) ->
      computer
      |> execute_binary_op(binary_op)
      |> result.try(run_program)

    Halt -> Ok(computer)
  }
}

fn binary_op_add(computer: Computer) -> Result(BinaryOp, Nil) {
  use #(a_addr, b_addr, dest_addr) <- result.try(
    computer |> binary_op_addresses,
  )

  Ok(BinaryOp(int.add, a_addr:, b_addr:, dest_addr:))
}

fn binary_op_multiply(computer: Computer) -> Result(BinaryOp, Nil) {
  use #(a_addr, b_addr, dest_addr) <- result.try(
    computer |> binary_op_addresses,
  )

  Ok(BinaryOp(int.multiply, a_addr:, b_addr:, dest_addr:))
}

fn binary_op_addresses(
  computer: Computer,
) -> Result(#(Address, Address, Address), Nil) {
  use a_addr <- result.try(computer |> read_address(ip_offset: 1))
  use b_addr <- result.try(computer |> read_address(ip_offset: 2))
  use dest_addr <- result.try(computer |> read_address(ip_offset: 3))

  Ok(#(a_addr, b_addr, dest_addr))
}

fn read_address(
  computer: Computer,
  ip_offset ip_offset: Int,
) -> Result(Address, Nil) {
  computer.instruction_pointer
  |> address_offset(ip_offset)
  |> peek_memory(computer, _)
  |> result.map(Address)
}

fn execute_binary_op(
  computer: Computer,
  op: BinaryOp,
) -> Result(Computer, Nil) {
  use a <- result.try(computer |> peek_memory(op.a_addr))
  use b <- result.try(computer |> peek_memory(op.b_addr))

  Ok(
    computer
    |> poke_memory(op.dest_addr, op.apply(a, b))
    |> ip_advance(by: 4),
  )
}

fn ip_advance(computer: Computer, by offset: Int) -> Computer {
  Computer(
    ..computer,
    instruction_pointer: address_offset(computer.instruction_pointer, offset),
  )
}
