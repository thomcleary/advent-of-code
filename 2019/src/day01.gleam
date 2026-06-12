import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub const part1_answer = 3_442_987

pub const part2_answer = 5_161_601

pub fn part1(input: String) -> Result(Int, String) {
  use masses <- result.map(parse_masses(input))

  masses
  |> list.map(required_fuel)
  |> int.sum
}

pub fn part2(input: String) -> Result(Int, String) {
  use masses <- result.map(parse_masses(input))

  masses
  |> list.map(required_fuels_fuel(_, 0))
  |> int.sum
}

fn parse_masses(input: String) -> Result(List(Int), String) {
  input
  |> string.trim
  |> string.split(on: "\n")
  |> list.try_map(int.parse)
  |> result.replace_error("Non integer mass found in input")
}

fn required_fuel(mass: Int) -> Int {
  { mass / 3 } - 2
}

fn required_fuels_fuel(mass: Int, fuel: Int) -> Int {
  let required = required_fuel(mass)

  case required > 0 {
    True -> required_fuels_fuel(required, fuel + required)
    False -> fuel
  }
}
