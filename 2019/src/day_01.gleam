import gleam/int
import gleam/io
import gleam/list
import gleam/string
import simplifile

const input_filepath = "./inputs/day_01.txt"

pub fn main() {
  let assert Ok(input) = simplifile.read(input_filepath)

  let assert Ok(masses) =
    input
    |> string.trim
    |> string.split(on: "\n")
    |> list.try_map(int.parse)

  masses
  |> part_1
  |> int.to_string
  |> string.append("Part 1: ", _)
  |> io.println

  masses
  |> part_2
  |> int.to_string
  |> string.append("Part 2: ", _)
  |> io.println
}

pub fn part_1(masses: List(Int)) -> Int {
  masses
  |> list.map(required_fuel)
  |> list.fold(0, int.add)
}

pub fn part_2(masses: List(Int)) -> Int {
  masses
  |> list.map(required_fuels_fuel(_, 0))
  |> list.fold(0, int.add)
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
