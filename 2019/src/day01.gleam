import gleam/int
import gleam/list
import gleam/string
import lib/part

pub fn solve(input: String) -> Nil {
  let assert Ok(masses) = parse_masses(input)

  masses
  |> part1
  |> part.print(part.One)

  masses
  |> part2
  |> part.print(part.Two)
}

pub fn part1(masses: List(Int)) -> Int {
  masses
  |> list.map(required_fuel)
  |> int.sum
}

pub fn part2(masses: List(Int)) -> Int {
  masses
  |> list.map(required_fuels_fuel(_, 0))
  |> int.sum
}

fn parse_masses(input: String) -> Result(List(Int), Nil) {
  input
  |> string.trim
  |> string.split(on: "\n")
  |> list.try_map(int.parse)
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
