import gleam/int
import gleam/list
import gleam/result
import gleam/string
import lib/intcode

pub const part1_answer = 3_512_778_005

pub const part2_answer = 35_920

pub fn part1(input: String) -> Result(String, String) {
  use boost_program <- result.try(
    intcode.parse_program(input) |> result.map_error(intcode.error_to_string),
  )

  boost_program
  |> run(in: TestMode)
}

pub fn part2(input: String) -> Result(String, String) {
  use boost_program <- result.try(
    intcode.parse_program(input) |> result.map_error(intcode.error_to_string),
  )

  boost_program
  |> run(in: BoostMode)
}

type Mode {
  TestMode
  BoostMode
}

fn run(program: intcode.Program, in mode: Mode) -> Result(String, String) {
  program
  |> intcode.boot
  |> intcode.with_input([
    case mode {
      TestMode -> 1
      BoostMode -> 2
    },
  ])
  |> intcode.run
  |> result.map_error(intcode.error_to_string)
  |> result.try(fn(computer) {
    case computer |> intcode.output {
      [output] -> Ok(output |> int.to_string)
      errors ->
        Error(
          "BOOST program reported errors: ["
          <> errors |> list.map(int.to_string) |> string.join(with: ",")
          <> "]",
        )
    }
  })
}
