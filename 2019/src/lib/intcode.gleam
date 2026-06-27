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

pub type Address {
  Address(value: Int)
}

pub type State {
  Booted
  Blocked
  Halted
}

pub opaque type Computer {
  Computer(
    state: State,
    memory: dict.Dict(Address, Int),
    relative_base: Int,
    instruction_pointer: Address,
    input: List(Int),
    output: List(Int),
    blocking_io: Bool,
  )
}

pub fn boot(program: Program) -> Computer {
  Computer(
    state: Booted,
    memory: program.code
      |> list.index_map(fn(integer, index) { #(Address(index), integer) })
      |> dict.from_list,
    relative_base: 0,
    instruction_pointer: Address(0),
    input: [],
    output: [],
    blocking_io: False,
  )
}

pub fn with_input(computer: Computer, input: List(Int)) -> Computer {
  Computer(..computer, input: list.append(computer.input, input))
}

pub fn with_blocking_io(computer: Computer) -> Computer {
  Computer(..computer, blocking_io: True)
}

pub fn state(computer: Computer) -> State {
  computer.state
}

pub fn output(computer: Computer) -> List(Int) {
  computer.output |> list.reverse
}

pub fn peek_memory(
  in computer: Computer,
  at address: Address,
) -> Result(Int, IntcodeError) {
  case address {
    a if a.value < 0 -> Error(SegmentationFault(computer:, at: address))
    _ -> Ok(result.unwrap(dict.get(computer.memory, address), or: 0))
  }
}

pub fn poke_memory(
  in computer: Computer,
  at address: Address,
  with value: Int,
) -> Result(Computer, IntcodeError) {
  case address {
    a if a.value < 0 -> Error(SegmentationFault(computer, at: address))
    _ ->
      Ok(
        Computer(
          ..computer,
          memory: dict.insert(computer.memory, for: address, insert: value),
        ),
      )
  }
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

pub fn run(computer: Computer) -> Result(Computer, IntcodeError) {
  use instruction <- result.try(
    computer
    |> peek_memory(at: computer.instruction_pointer),
  )

  let opcode = instruction % 100

  case opcode {
    1 | 2 -> {
      use parameters <- result.try(
        computer
        |> parse_binary_op_parameters(from: instruction),
      )

      let op = case opcode {
        1 -> add
        _ -> multiply
      }

      computer
      |> op(parameters)
      |> result.try(run)
    }

    3 -> {
      case
        computer |> parse_read_parameters(from: instruction),
        computer.blocking_io
      {
        Ok(parameters), _ ->
          computer
          |> read(with: parameters)
          |> result.try(run)

        Error(EndOfInput(_)), True -> Ok(computer |> block)
        Error(err), _ -> Error(err)
      }
    }

    4 -> {
      use parameters <- result.try(
        computer
        |> parse_write_parameters(from: instruction),
      )

      use computer <- result.try(computer |> write(with: parameters))

      case computer.blocking_io {
        True -> Ok(computer |> block)
        False -> computer |> run
      }
    }

    5 | 6 -> {
      use parameters <- result.try(
        computer
        |> parse_jump_parameters(from: instruction),
      )

      computer
      |> jump_if(opcode == 5, with: parameters)
      |> result.try(run)
    }

    7 | 8 -> {
      use parameters <- result.try(
        computer
        |> parse_binary_op_parameters(from: instruction),
      )

      computer
      |> compare(
        by: case opcode {
          7 -> order.Lt
          _ -> order.Eq
        },
        with: parameters,
      )
      |> result.try(run)
    }

    9 -> {
      use parameters <- result.try(
        computer
        |> parse_adjust_relative_base_parameters(from: instruction),
      )

      computer
      |> adjust_relative_base(with: parameters)
      |> result.try(run)
    }

    99 -> computer |> halt |> Ok

    _ -> Error(InvalidInstruction(computer:, instruction: instruction))
  }
}

type ParameterMode {
  PositionMode
  ImmediateMode
  RelativeMode
}

type Parameter {
  PositionModeParameter(address: Address)
  ImmediateModeParameter(value: Int)
  RelativeModeParameter(offset: Int)
}

type BinaryOpParameters {
  BinaryOpParameters(a: Parameter, b: Parameter, store_in: Address)
}

type ReadParameters {
  ReadParameters(value: Int, into: Address)
}

type WriteParameters {
  WriteParameters(with: Parameter)
}

type JumpParameters {
  JumpParameters(condition: Parameter, to: Parameter)
}

type AdjustRelativeBaseParameters {
  AdjustRelativeBaseParameters(offset: Parameter)
}

fn address_at_offset(by offset: Int, from address: Address) -> Address {
  Address(address.value + offset)
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
      2 -> Ok(RelativeMode)
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
    RelativeMode -> RelativeModeParameter(offset: value)
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
  use c <- result.try(get_parameter(3, from: computer, with: parameter_modes))

  use store_in <- result.map(case c {
    PositionModeParameter(address:) -> Ok(address)
    RelativeModeParameter(offset:) ->
      Ok(Address(computer.relative_base + offset))
    ImmediateModeParameter(_) ->
      Error(InvalidInstruction(computer:, instruction:))
  })

  BinaryOpParameters(a:, b:, store_in:)
}

fn parse_read_parameters(
  computer: Computer,
  from instruction: Int,
) -> Result(ReadParameters, IntcodeError) {
  use parameter_modes <- result.try(parse_parameter_modes(
    from: instruction,
    with: computer,
  ))

  use parameter <- result.try(get_parameter(
    1,
    from: computer,
    with: parameter_modes,
  ))

  use address <- result.try(case parameter {
    PositionModeParameter(address:) -> Ok(address)
    RelativeModeParameter(offset:) ->
      Ok(Address(computer.relative_base + offset))
    ImmediateModeParameter(_) ->
      Error(InvalidInstruction(computer:, instruction:))
  })

  use value <- result.map(
    computer.input
    |> list.first
    |> result.replace_error(EndOfInput(computer:)),
  )

  ReadParameters(value:, into: address)
}

fn parse_write_parameters(
  computer: Computer,
  from instruction: Int,
) -> Result(WriteParameters, IntcodeError) {
  use parameter_modes <- result.try(parse_parameter_modes(
    from: instruction,
    with: computer,
  ))

  use parameter <- result.map(get_parameter(
    1,
    from: computer,
    with: parameter_modes,
  ))

  WriteParameters(with: parameter)
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

fn parse_adjust_relative_base_parameters(
  computer: Computer,
  from instruction: Int,
) -> Result(AdjustRelativeBaseParameters, IntcodeError) {
  use parameter_modes <- result.try(parse_parameter_modes(
    from: instruction,
    with: computer,
  ))

  use offset <- result.map(get_parameter(
    1,
    from: computer,
    with: parameter_modes,
  ))

  AdjustRelativeBaseParameters(offset:)
}

fn get_value_of_parameter(
  computer: Computer,
  parameter: Parameter,
) -> Result(Int, IntcodeError) {
  case parameter {
    PositionModeParameter(address:) -> peek_memory(in: computer, at: address)
    ImmediateModeParameter(value:) -> Ok(value)
    RelativeModeParameter(offset:) ->
      peek_memory(in: computer, at: Address(computer.relative_base + offset))
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
  use b <- result.try(computer |> get_value_of_parameter(parameters.b))

  use computer <- result.map(poke_memory(
    in: computer,
    at: parameters.store_in,
    with: apply(a, b),
  ))

  computer |> increase_instruction_pointer(by: 4)
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
  with parameters: ReadParameters,
) -> Result(Computer, IntcodeError) {
  let computer = Computer(..computer, input: computer.input |> list.drop(1))

  use computer <- result.map(poke_memory(
    in: computer,
    at: parameters.into,
    with: parameters.value,
  ))

  computer |> increase_instruction_pointer(by: 2)
}

fn write(
  computer: Computer,
  with parameters: WriteParameters,
) -> Result(Computer, IntcodeError) {
  use value <- result.map(computer |> get_value_of_parameter(parameters.with))

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

fn adjust_relative_base(
  computer: Computer,
  with parameters: AdjustRelativeBaseParameters,
) -> Result(Computer, IntcodeError) {
  use offset <- result.map(
    computer |> get_value_of_parameter(parameters.offset),
  )

  Computer(..computer, relative_base: computer.relative_base + offset)
  |> increase_instruction_pointer(by: 2)
}

fn block(computer: Computer) -> Computer {
  Computer(..computer, state: Blocked)
}

fn halt(computer: Computer) -> Computer {
  Computer(..computer, state: Halted)
}
