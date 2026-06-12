import gleam/int
import gleam/list
import gleam/result
import gleam/string
import lib/part

pub fn solve(input: String) -> Nil {
  let assert Ok(password_range) = parse_password_range(input)

  password_range
  |> part1
  |> result.map(int.to_string)
  |> part.try_print(part.One)

  part.to_do
  |> Ok
  |> part.try_print(part.Two)
}

type Digits {
  Digits(
    first: Int,
    second: Int,
    third: Int,
    fourth: Int,
    fifth: Int,
    sixth: Int,
  )
}

pub type PasswordRange {
  PasswordRange(min: Int, max: Int)
}

pub fn part1(range: PasswordRange) -> Result(Int, Nil) {
  count_valid_passwords(from: range.min, to: range.max, count: 0)
}

fn count_valid_passwords(
  from password: Int,
  to max: Int,
  count count: Int,
) -> Result(Int, Nil) {
  case password > max {
    True -> Ok(count)
    False ->
      case is_valid_password(password) {
        Ok(True) ->
          count_valid_passwords(from: password + 1, to: max, count: count + 1)
        Ok(False) -> count_valid_passwords(from: password + 1, to: max, count:)
        Error(err) -> Error(err)
      }
  }
}

fn parse_password_range(input) -> Result(PasswordRange, Nil) {
  use #(first, second) <- result.try(string.split_once(input, on: "-"))
  use min <- result.try(int.parse(first))
  use max <- result.map(int.parse(second))

  PasswordRange(min:, max:)
}

fn is_valid_password(password: Int) -> Result(Bool, Nil) {
  use digits <- result.map(parse_digits(password))

  let adjacent = has_two_adjacent_digits_that_are_the_same(digits)
  let non_decreasing = is_non_decreasing(digits)

  adjacent && non_decreasing
}

fn parse_digits(n: Int) -> Result(Digits, Nil) {
  let digits =
    n
    |> int.to_string
    |> string.split(on: "")
    |> list.try_map(int.parse)

  case digits {
    Ok([first, second, third, fourth, fifth, sixth]) ->
      Ok(Digits(first:, second:, third:, fourth:, fifth:, sixth:))
    _ -> Error(Nil)
  }
}

fn has_two_adjacent_digits_that_are_the_same(digits: Digits) -> Bool {
  case digits {
    Digits(first, second, _, _, _, _) if first == second -> True
    Digits(_, second, third, _, _, _) if second == third -> True
    Digits(_, _, third, fourth, _, _) if third == fourth -> True
    Digits(_, _, _, fourth, fifth, _) if fourth == fifth -> True
    Digits(_, _, _, _, fifth, sixth) if fifth == sixth -> True
    _ -> False
  }
}

fn is_non_decreasing(digits: Digits) -> Bool {
  let Digits(first:, second:, third:, fourth:, fifth:, sixth:) = digits

  first <= second
  && second <= third
  && third <= fourth
  && fourth <= fifth
  && fifth <= sixth
}
