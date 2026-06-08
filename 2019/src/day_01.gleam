import gleam/int
import gleam/io
import gleam/list
import gleam/string
import simplifile

const input_filepath = "./inputs/day_01.txt"

pub fn main() {
  let assert Ok(input) = simplifile.read(input_filepath)
  let assert Ok(masses) = to_masses(input)

  masses
  |> part1
  |> int.to_string
  |> string.append("Part 1: ", _)
  |> io.println
}

pub fn part1(masses: List(Int)) -> Int {
  list.fold(masses, 0, fn(sum, mass) { { { mass / 3 } - 2 } + sum })
}

fn to_masses(input: String) -> Result(List(Int), Nil) {
  string.split(input, on: "\n") |> list.try_map(int.parse)
}
