import day01
import day02
import day03
import day04
import day05
import gleam/erlang/application
import gleam/int
import gleam/option
import gleam/result
import gleam/time/duration
import gleam/time/timestamp
import simplifile

const application_name = "aoc2019"

pub type Day {
  Day01
  Day02
  Day03
  Day04
  Day05
}

pub type PartFn =
  fn(String) -> Result(Int, String)

pub const solved_days = [Day01, Day02, Day03, Day04, Day05]

pub fn parse_day(str: String) -> Result(Day, Nil) {
  case str {
    "1" | "01" -> Ok(Day01)
    "2" | "02" -> Ok(Day02)
    "3" | "03" -> Ok(Day03)
    "4" | "04" -> Ok(Day04)
    "5" | "05" -> Ok(Day05)
    _ -> Error(Nil)
  }
}

pub fn day_to_parts(day: Day) -> #(PartFn, PartFn) {
  case day {
    Day01 -> #(day01.part1, day01.part2)
    Day02 -> #(day02.part1, day02.part2)
    Day03 -> #(day03.part1, day03.part2)
    Day04 -> #(day04.part1, day04.part2)
    Day05 -> #(day05.part1, day05.part2)
  }
}

fn day_to_answers(day: Day) -> #(option.Option(Int), option.Option(Int)) {
  case day {
    Day01 -> #(option.Some(day01.part1_answer), option.Some(day01.part2_answer))
    Day02 -> #(option.Some(day02.part1_answer), option.Some(day02.part2_answer))
    Day03 -> #(option.Some(day03.part1_answer), option.Some(day03.part2_answer))
    Day04 -> #(option.Some(day04.part1_answer), option.Some(day04.part2_answer))
    Day05 -> #(option.Some(day05.part1_answer), option.Some(day05.part2_answer))
  }
}

pub fn day_to_string(day: Day) -> String {
  case day {
    Day01 -> "01"
    Day02 -> "02"
    Day03 -> "03"
    Day04 -> "04"
    Day05 -> "05"
  }
}

pub fn read_input(day: Day) -> Result(String, String) {
  use priv <- result.try(
    application.priv_directory(application_name)
    |> result.replace_error(
      "No application with name [" <> application_name <> "] found",
    ),
  )
  let file_path = priv <> "/inputs/day" <> day_to_string(day) <> ".txt"

  simplifile.read(file_path)
  |> result.replace_error("Failed to read input file [" <> file_path <> "]")
}

pub type PartAssertion {
  Todo(value: String)
  Pass(value: String)
  Fail(actual: String, expected: String)
}

pub type PartRun {
  PartRun(result: Result(PartAssertion, String), time: Int)
}

pub type DayRun {
  DayRun(day: Day, part1: PartRun, part2: PartRun)
}

pub fn run_day(day: Day) -> Result(DayRun, String) {
  use input <- result.try(read_input(day))

  let #(part1, part2) = day_to_parts(day)
  let #(part1_expected, part2_expected) = day_to_answers(day)

  Ok(DayRun(
    day:,
    part1: run_part(part1, with: input, expected: part1_expected),
    part2: run_part(part2, with: input, expected: part2_expected),
  ))
}

fn run_part(
  part: PartFn,
  with input: String,
  expected expected: option.Option(Int),
) -> PartRun {
  let start = timestamp.system_time()
  let part_result = part(input)
  let end = timestamp.system_time()

  let time =
    timestamp.difference(start, end)
    |> duration.to_milliseconds

  let assertion =
    result.map(part_result, fn(actual) {
      let actual = int.to_string(actual)
      case expected {
        option.None -> Todo(value: actual)
        option.Some(expected) -> {
          let expected = int.to_string(expected)
          case actual == expected {
            True -> Pass(value: actual)
            False -> Fail(actual:, expected:)
          }
        }
      }
    })

  PartRun(result: assertion, time:)
}
