import argv
import day01
import day02
import day03
import gleam/erlang/application
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import gleam/time/duration
import gleam/time/timestamp
import lib/term
import simplifile

pub fn main() -> Nil {
  case argv.load().arguments {
    [] -> list.each(solved_days, solve)
    [first, ..] ->
      case parse_day(first) {
        Ok(day) -> solve(day)
        Error(_) -> usage()
      }
  }
}

const application_name = "aoc2019"

type Day {
  Day01
  Day02
  Day03
}

const solved_days = [Day01, Day02, Day03]

fn parse_day(str: String) -> Result(Day, Nil) {
  case str {
    "1" | "01" -> Ok(Day01)
    "2" | "02" -> Ok(Day02)
    "3" | "03" -> Ok(Day03)
    _ -> Error(Nil)
  }
}

fn day_to_string(day: Day) -> String {
  case day {
    Day01 -> "01"
    Day02 -> "02"
    Day03 -> "03"
  }
}

fn usage() -> Nil {
  io.println("")

  "usage:"
  |> term.escape(term.FgMagenta)
  |> string.append(" ")
  |> string.append("gleam run" |> term.escape(term.Bold))
  |> string.append(" ")
  |> string.append("[{" |> term.escape(term.Faint))
  |> string.append(
    solved_days
    |> list.map(fn(day) { day_to_string(day) |> term.escape(term.FgYellow) })
    |> string.join("|" |> term.escape(term.Faint)),
  )
  |> string.append("}]" |> term.escape(term.Faint))
  |> io.println
}

fn solve(day: Day) -> Nil {
  let columns = result.unwrap(term.columns(), or: 0)
  let width = int.min(columns, 40)

  io.println("")

  { "\u{250C}" <> " Day " <> day_to_string(day) <> " " }
  |> string.pad_end(to: width - 1, with: "\u{2500}")
  |> string.append("\u{2510}")
  |> term.escape(term.Bold)
  |> term.escape(term.FgMagenta)
  |> io.println

  let and_solve = case day {
    Day01 -> day01.solve
    Day02 -> day02.solve
    Day03 -> day03.solve
  }

  let start = timestamp.system_time()

  day
  |> read_input
  |> and_solve

  let time_taken =
    start
    |> timestamp.difference(timestamp.system_time())
    |> duration.to_milliseconds

  "\u{2514}"
  |> string.append(" ")
  |> string.append(int.to_string(time_taken))
  |> string.append("ms")
  |> term.escape(term.FgMagenta)
  |> io.println
}

fn read_input(day: Day) -> String {
  let assert Ok(priv) = application.priv_directory(application_name)

  let file_path = priv <> "/inputs/day" <> day_to_string(day) <> ".txt"

  case simplifile.read(file_path) {
    Ok(input) -> input
    Error(_) -> panic as { "Failed to read input file [" <> file_path <> "]" }
  }
}
