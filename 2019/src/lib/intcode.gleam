import gleam/dict
import gleam/int
import gleam/list
import gleam/order
import gleam/result
import gleam/string
import lib/digits

pub type Program {
  Program(code: List(Int))
}

pub type Address {
  Address(value: Int)
}

pub opaque type Computer {
  Computer(
    memory: dict.Dict(Address, Int),
    instruction_pointer: Address,
    input: List(Int),
    output: List(Int),
  )
}

type ParameterMode {
  PositionMode
  ImmediateMode
}

type Parameter {
  PositionModeParameter(address: Address)
  ImmediateModeParameter(value: Int)
}

type BinaryOpParameters {
  BinaryOpParameters(a: Parameter, b: Parameter, store_in: Address)
}

type JumpParameters {
  JumpParameters(condition: Parameter, to: Parameter)
}

type Instruction {
  Add(with: BinaryOpParameters)
  Multiply(with: BinaryOpParameters)
  Read(into: Address, with: Int)
  Write(with: Parameter)
  JumpIfTrue(with: JumpParameters)
  JumpIfFalse(with: JumpParameters)
  LessThan(with: BinaryOpParameters)
  Equals(with: BinaryOpParameters)
  Halt
}

pub type IntcodeError {
  ParseError
  SegmentationFault(computer: Computer, at: Address)
  InvalidInstruction(computer: Computer, instruction: Int)
  EndOfInput(computer: Computer)
}

pub fn error_to_string(err: IntcodeError) -> String {
  case err {
    ParseError -> "Failed to parse program"
    SegmentationFault(_, at: address) ->
      "Segmentation fault at address [" <> int.to_string(address.value) <> "]"
    InvalidInstruction(_, instruction:) ->
      "Invalid instruction [" <> int.to_string(instruction) <> "]"
    EndOfInput(_) -> "End of input"
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
  in computer: Computer,
  at address: Address,
) -> Result(Int, IntcodeError) {
  dict.get(computer.memory, address)
  |> result.replace_error(SegmentationFault(computer:, at: address))
}

pub fn poke_memory(
  in computer: Computer,
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
    input: [],
    output: [],
  )
}

pub fn with_input(computer: Computer, input: List(Int)) -> Computer {
  Computer(..computer, input:)
}

pub fn output(computer: Computer) -> List(Int) {
  computer.output
}

pub fn run(computer: Computer) -> Result(Computer, IntcodeError) {
  use instruction <- result.try(
    computer
    |> peek_memory(at: computer.instruction_pointer)
    |> result.try(parse_instruction(from: computer, with: _)),
  )

  case instruction {
    Add(parameters) ->
      computer
      |> add(with: parameters)
      |> result.try(run)

    Multiply(parameters) ->
      computer
      |> multiply(with: parameters)
      |> result.try(run)

    Read(address, value) -> {
      computer
      |> read(into: address, with: value)
      |> run
    }

    Write(parameter) ->
      computer
      |> write(with: parameter)
      |> result.try(run)

    JumpIfTrue(with: parameters) ->
      computer
      |> jump_if(True, with: parameters)
      |> result.try(run)

    JumpIfFalse(with: parameters) ->
      computer
      |> jump_if(False, with: parameters)
      |> result.try(run)

    LessThan(with: parameters) ->
      computer
      |> compare(by: order.Lt, with: parameters)
      |> result.try(run)

    Equals(with: parameters) ->
      computer
      |> compare(by: order.Eq, with: parameters)
      |> result.try(run)

    Halt -> Ok(computer)
  }
}

fn parse_instruction(
  from computer: Computer,
  with instruction: Int,
) -> Result(Instruction, IntcodeError) {
  case parse_opcode(instruction) {
    1 ->
      computer
      |> parse_binary_op_parameters(from: instruction)
      |> result.map(Add)

    2 ->
      computer
      |> parse_binary_op_parameters(from: instruction)
      |> result.map(Multiply)

    3 ->
      computer
      |> parse_read_instruction

    4 ->
      computer
      |> parse_write_instruction(from: instruction)

    5 ->
      computer
      |> parse_jump_parameters(from: instruction)
      |> result.map(JumpIfTrue)

    6 ->
      computer
      |> parse_jump_parameters(from: instruction)
      |> result.map(JumpIfFalse)

    7 ->
      computer
      |> parse_binary_op_parameters(from: instruction)
      |> result.map(LessThan)

    8 ->
      computer
      |> parse_binary_op_parameters(from: instruction)
      |> result.map(Equals)

    99 -> Ok(Halt)

    _ -> Error(InvalidInstruction(computer:, instruction: instruction))
  }
}

fn parse_opcode(instruction: Int) -> Int {
  instruction % 100
}

fn parse_parameter_modes(
  from instruction: Int,
  with computer: Computer,
) -> Result(dict.Dict(Int, ParameterMode), IntcodeError) {
  instruction / 100
  |> digits.from_int
  |> list.reverse
  |> list.try_map(with: fn(mode) {
    case mode {
      0 -> Ok(PositionMode)
      1 -> Ok(ImmediateMode)
      _ -> Error(InvalidInstruction(computer:, instruction:))
    }
  })
  |> result.map(
    list.index_fold(_, from: dict.new(), with: fn(params, mode, i) {
      dict.insert(into: params, for: i + 1, insert: mode)
    }),
  )
}

fn get_parameter(
  parameter: Int,
  from computer: Computer,
  with parameter_modes: dict.Dict(Int, ParameterMode),
) -> Result(Parameter, IntcodeError) {
  use value <- result.map(peek_memory(
    in: computer,
    at: address_at_offset(parameter, from: computer.instruction_pointer),
  ))

  let mode =
    parameter_modes
    |> dict.get(parameter)
    |> result.unwrap(PositionMode)

  case mode {
    PositionMode -> PositionModeParameter(address: Address(value))
    ImmediateMode -> ImmediateModeParameter(value: value)
  }
}

fn parse_binary_op_parameters(
  computer: Computer,
  from instruction: Int,
) -> Result(BinaryOpParameters, IntcodeError) {
  use parameter_modes <- result.try(parse_parameter_modes(
    from: instruction,
    with: computer,
  ))

  use a <- result.try(get_parameter(1, from: computer, with: parameter_modes))
  use b <- result.try(get_parameter(2, from: computer, with: parameter_modes))

  use store_in <- result.map(
    computer
    |> peek_memory(at: address_at_offset(3, from: computer.instruction_pointer))
    |> result.map(Address),
  )

  BinaryOpParameters(a:, b:, store_in:)
}

fn parse_read_instruction(
  computer: Computer,
) -> Result(Instruction, IntcodeError) {
  use value <- result.try(
    computer.input
    |> list.first
    |> result.replace_error(EndOfInput(computer:)),
  )

  use parameter <- result.map(
    computer
    |> peek_memory(at: address_at_offset(
      by: 1,
      from: computer.instruction_pointer,
    )),
  )

  Read(into: Address(parameter), with: value)
}

fn parse_write_instruction(
  computer: Computer,
  from instruction: Int,
) -> Result(Instruction, IntcodeError) {
  use parameter_modes <- result.try(parse_parameter_modes(
    from: instruction,
    with: computer,
  ))

  use parameter <- result.map(get_parameter(
    1,
    from: computer,
    with: parameter_modes,
  ))

  Write(with: parameter)
}

fn parse_jump_parameters(
  computer: Computer,
  from instruction: Int,
) -> Result(JumpParameters, IntcodeError) {
  use parameter_modes <- result.try(parse_parameter_modes(
    from: instruction,
    with: computer,
  ))

  use condition <- result.try(get_parameter(
    1,
    from: computer,
    with: parameter_modes,
  ))

  use to <- result.map(get_parameter(2, from: computer, with: parameter_modes))

  JumpParameters(condition:, to:)
}

fn get_value_of_parameter(
  computer: Computer,
  parameter: Parameter,
) -> Result(Int, IntcodeError) {
  case parameter {
    PositionModeParameter(address:) -> peek_memory(in: computer, at: address)
    ImmediateModeParameter(value:) -> Ok(value)
  }
}

fn increase_instruction_pointer(
  computer: Computer,
  by offset: Int,
) -> Computer {
  Computer(
    ..computer,
    instruction_pointer: address_at_offset(
      offset,
      from: computer.instruction_pointer,
    ),
  )
}

fn run_binary_op_instruction(
  computer: Computer,
  with parameters: BinaryOpParameters,
  and apply: fn(Int, Int) -> Int,
) -> Result(Computer, IntcodeError) {
  use a <- result.try(computer |> get_value_of_parameter(parameters.a))
  use b <- result.map(computer |> get_value_of_parameter(parameters.b))

  computer
  |> poke_memory(at: parameters.store_in, with: apply(a, b))
  |> increase_instruction_pointer(by: 4)
}

fn add(
  computer: Computer,
  with parameters: BinaryOpParameters,
) -> Result(Computer, IntcodeError) {
  computer |> run_binary_op_instruction(with: parameters, and: int.add)
}

fn multiply(
  computer: Computer,
  with parameters: BinaryOpParameters,
) -> Result(Computer, IntcodeError) {
  computer |> run_binary_op_instruction(with: parameters, and: int.multiply)
}

fn read(
  computer: Computer,
  into address: Address,
  with value: Int,
) -> Computer {
  Computer(..computer, input: computer.input |> list.drop(1))
  |> poke_memory(at: address, with: value)
  |> increase_instruction_pointer(by: 2)
}

fn write(
  computer: Computer,
  with parameter: Parameter,
) -> Result(Computer, IntcodeError) {
  use value <- result.map(computer |> get_value_of_parameter(parameter))

  Computer(..computer, output: [value, ..computer.output])
  |> increase_instruction_pointer(by: 2)
}

fn jump_if(
  computer: Computer,
  jump_type: Bool,
  with parameters: JumpParameters,
) -> Result(Computer, IntcodeError) {
  use condition <- result.try(
    computer |> get_value_of_parameter(parameters.condition),
  )

  let jump = fn() {
    use to <- result.map(computer |> get_value_of_parameter(parameters.to))
    Computer(..computer, instruction_pointer: Address(to))
  }

  let next = fn() {
    Ok(
      computer
      |> increase_instruction_pointer(by: 3),
    )
  }

  case jump_type, condition {
    True, 0 -> next()
    True, _ -> jump()
    False, 0 -> jump()
    False, _ -> next()
  }
}

fn compare(
  computer: Computer,
  by order: order.Order,
  with parameters: BinaryOpParameters,
) -> Result(Computer, IntcodeError) {
  let apply = fn(a, b) {
    case int.compare(a, b) == order {
      True -> 1
      False -> 0
    }
  }

  computer
  |> run_binary_op_instruction(with: parameters, and: apply)
}
