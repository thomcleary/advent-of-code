import argv
import day01
import day02
import day03
import gleam/erlang/application
import gleam/int
import gleam/io
import gleam/string
import simplifile

const application_name = "aoc2019"

pub fn main() -> Nil {
  case argv.load().arguments {
    ["1"] | ["01"] -> day01.solve |> with_puzzle_input(01)
    ["2"] | ["02"] -> day02.solve |> with_puzzle_input(02)
    ["3"] | ["03"] -> day03.solve |> with_puzzle_input(03)
    _ -> io.println("usage: gleam run [day]")
  }
}

fn with_puzzle_input(run: fn(String) -> Nil, day: Int) -> Nil {
  read_input(day:) |> run
}

fn read_input(day day: Int) -> String {
  let assert Ok(priv) = application.priv_directory(application_name)

  let day =
    day
    |> int.to_string
    |> string.pad_start(to: 2, with: "0")

  let file_path = priv <> "/inputs/day" <> day <> ".txt"

  case simplifile.read(file_path) {
    Ok(input) -> input
    Error(_) -> panic as { "Failed to read input file [" <> file_path <> "]" }
  }
}
