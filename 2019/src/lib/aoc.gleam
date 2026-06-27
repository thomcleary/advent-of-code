import day01
import day02
import day03
import day04
import day05
import day06
import day07
import day08
import day09
import gleam/erlang/application
import gleam/int
import gleam/option
import gleam/result
import gleam/time/duration
import gleam/time/timestamp
import simplifile

const application_name = "aoc2019"

pub type PartFn =
  fn(String) -> Result(String, String)

pub type Day {
  Day01
  Day02
  Day03
  Day04
  Day05
  Day06
  Day07
  Day08
  Day09
  // Day10
  // Day11
  // Day12
  // Day13
  // Day14
  // Day15
  // Day16
  // Day17
  // Day18
  // Day19
  // Day20
  // Day21
  // Day22
  // Day23
  // Day24
  // Day25
}

pub const solved_days = [
  Day01,
  Day02,
  Day03,
  Day04,
  Day05,
  Day06,
  Day07,
  Day08,
  Day09,
  // Day10,
// Day11,
// Day12,
// Day13,
// Day14,
// Day15,
// Day16,
// Day17,
// Day18,
// Day19,
// Day20,
// Day21,
// Day22,
// Day23,
// Day24,
// Day25,
]

pub fn parse_day(str: String) -> Result(Day, Nil) {
  case str {
    "1" | "01" -> Ok(Day01)
    "2" | "02" -> Ok(Day02)
    "3" | "03" -> Ok(Day03)
    "4" | "04" -> Ok(Day04)
    "5" | "05" -> Ok(Day05)
    "6" | "06" -> Ok(Day06)
    "7" | "07" -> Ok(Day07)
    "8" | "08" -> Ok(Day08)
    "9" | "09" -> Ok(Day09)
    // "10" -> Ok(Day10)
    // "11" -> Ok(Day11)
    // "12" -> Ok(Day12)
    // "13" -> Ok(Day13)
    // "14" -> Ok(Day14)
    // "15" -> Ok(Day15)
    // "16" -> Ok(Day16)
    // "17" -> Ok(Day17)
    // "18" -> Ok(Day18)
    // "19" -> Ok(Day19)
    // "20" -> Ok(Day20)
    // "21" -> Ok(Day21)
    // "22" -> Ok(Day22)
    // "23" -> Ok(Day23)
    // "24" -> Ok(Day24)
    // "25" -> Ok(Day25)
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
    Day06 -> #(day06.part1, day06.part2)
    Day07 -> #(day07.part1, day07.part2)
    Day08 -> #(day08.part1, day08.part2)
    Day09 -> #(day09.part1, day09.part2)
    // Day10 -> todo
    // Day11 -> todo
    // Day12 -> todo
    // Day13 -> todo
    // Day14 -> todo
    // Day15 -> todo
    // Day16 -> todo
    // Day17 -> todo
    // Day18 -> todo
    // Day19 -> todo
    // Day20 -> todo
    // Day21 -> todo
    // Day22 -> todo
    // Day23 -> todo
    // Day24 -> todo
    // Day25 -> todo
  }
}

fn day_to_answers(day: Day) -> #(option.Option(Int), option.Option(Int)) {
  case day {
    Day01 -> #(option.Some(day01.part1_answer), option.Some(day01.part2_answer))
    Day02 -> #(option.Some(day02.part1_answer), option.Some(day02.part2_answer))
    Day03 -> #(option.Some(day03.part1_answer), option.Some(day03.part2_answer))
    Day04 -> #(option.Some(day04.part1_answer), option.Some(day04.part2_answer))
    Day05 -> #(option.Some(day05.part1_answer), option.Some(day05.part2_answer))
    Day06 -> #(option.Some(day06.part1_answer), option.Some(day06.part2_answer))
    Day07 -> #(option.Some(day07.part1_answer), option.Some(day07.part2_answer))
    Day08 -> #(option.Some(day08.part1_answer), option.None)
    Day09 -> #(option.Some(day09.part1_answer), option.Some(day09.part2_answer))
    // Day10 -> #(option.None, option.None)
    // Day11 -> #(option.None, option.None)
    // Day12 -> #(option.None, option.None)
    // Day13 -> #(option.None, option.None)
    // Day14 -> #(option.None, option.None)
    // Day15 -> #(option.None, option.None)
    // Day16 -> #(option.None, option.None)
    // Day17 -> #(option.None, option.None)
    // Day18 -> #(option.None, option.None)
    // Day19 -> #(option.None, option.None)
    // Day20 -> #(option.None, option.None)
    // Day21 -> #(option.None, option.None)
    // Day22 -> #(option.None, option.None)
    // Day23 -> #(option.None, option.None)
    // Day24 -> #(option.None, option.None)
    // Day25 -> #(option.None, option.None)
  }
}

pub fn day_to_string(day: Day) -> String {
  case day {
    Day01 -> "01"
    Day02 -> "02"
    Day03 -> "03"
    Day04 -> "04"
    Day05 -> "05"
    Day06 -> "06"
    Day07 -> "07"
    Day08 -> "08"
    Day09 -> "09"
    // Day10 -> "10"
    // Day11 -> "11"
    // Day12 -> "12"
    // Day13 -> "13"
    // Day14 -> "14"
    // Day15 -> "15"
    // Day16 -> "16"
    // Day17 -> "17"
    // Day18 -> "18"
    // Day19 -> "19"
    // Day20 -> "20"
    // Day21 -> "21"
    // Day22 -> "22"
    // Day23 -> "23"
    // Day24 -> "24"
    // Day25 -> "25"
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
