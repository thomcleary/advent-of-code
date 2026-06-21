import gleam/int
import gleam/list
import gleam/result
import gleam/string
import lib/digits

pub const part1_answer = 1048

pub const part2_answer = 677

pub fn part1(input: String) -> Result(Int, String) {
  use password_range <- result.try(parse_password_range(input))

  password_range
  |> count_valid_passwords(with: [
    is_non_decreasing,
    has_adjacent_pair,
  ])
}

pub fn part2(input: String) -> Result(Int, String) {
  use password_range <- result.try(parse_password_range(input))

  password_range
  |> count_valid_passwords(with: [
    is_non_decreasing,
    has_adjacent_pair_with_exactly_two_digits,
  ])
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

fn count_valid_passwords(
  range: PasswordRange,
  with predicates: List(fn(Digits) -> Bool),
) {
  do_count_valid_passwords(
    predicates:,
    from: range.min,
    to: range.max,
    count: 0,
  )
}

fn do_count_valid_passwords(
  predicates predicates: List(fn(Digits) -> Bool),
  from password: Int,
  to max: Int,
  count count: Int,
) -> Result(Int, String) {
  case password > max {
    True -> Ok(count)
    False -> {
      use password_digits <- result.try(case digits.from_int(password) {
        [first, second, third, fourth, fifth, sixth] ->
          Ok(Digits(first:, second:, third:, fourth:, fifth:, sixth:))
        _ ->
          Error(
            "Password ["
            <> int.to_string(password)
            <> "] does not have six digits",
          )
      })

      let count = case
        list.all(predicates, fn(pred) { pred(password_digits) })
      {
        True -> count + 1
        False -> count
      }

      do_count_valid_passwords(predicates:, from: password + 1, to: max, count:)
    }
  }
}

fn parse_password_range(input) -> Result(PasswordRange, String) {
  use #(first, second) <- result.try(
    string.split_once(input, on: "-")
    |> result.replace_error("Invalid range [" <> input <> "]"),
  )

  use min <- result.try(
    int.parse(first)
    |> result.replace_error("Invalid range minimum [" <> first <> "]"),
  )
  use max <- result.map(
    int.parse(second)
    |> result.replace_error("Invalid range minimum [" <> first <> "]"),
  )

  PasswordRange(min:, max:)
}

fn is_non_decreasing(digits: Digits) -> Bool {
  let Digits(first:, second:, third:, fourth:, fifth:, sixth:) = digits

  first <= second
  && second <= third
  && third <= fourth
  && fourth <= fifth
  && fifth <= sixth
}

fn has_adjacent_pair(digits: Digits) -> Bool {
  case digits {
    Digits(first, second, _, _, _, _) if first == second -> True
    Digits(_, second, third, _, _, _) if second == third -> True
    Digits(_, _, third, fourth, _, _) if third == fourth -> True
    Digits(_, _, _, fourth, fifth, _) if fourth == fifth -> True
    Digits(_, _, _, _, fifth, sixth) if fifth == sixth -> True
    _ -> False
  }
}

fn has_adjacent_pair_with_exactly_two_digits(digits: Digits) -> Bool {
  case digits {
    Digits(first, second, third, _, _, _)
      if first == second && second != third
    -> True
    Digits(first, second, third, fourth, _, _)
      if first != second && second == third && third != fourth
    -> True
    Digits(_, second, third, fourth, fifth, _)
      if second != third && third == fourth && fourth != fifth
    -> True
    Digits(_, _, third, fourth, fifth, sixth)
      if third != fourth && fourth == fifth && fifth != sixth
    -> True
    Digits(_, _, _, fourth, fifth, sixth)
      if fourth != fifth && fifth == sixth
    -> True
    _ -> False
  }
}
