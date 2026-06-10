import gleam/int
import gleam/io
import gleam/list
import gleam/string

pub fn solve(input: String) -> Nil {
  let assert Ok(masses) =
    input
    |> string.trim
    |> string.split(on: "\n")
    |> list.try_map(int.parse)

  masses
  |> part1
  |> int.to_string
  |> string.append("Part 1: ", _)
  |> io.println

  masses
  |> part2
  |> int.to_string
  |> string.append("Part 2: ", _)
  |> io.println
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
