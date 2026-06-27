import gleam/int
import gleam/list
import gleam/option
import gleam/set
import gleam/string

pub const part1_answer = 247

pub fn part1(input: String) -> Result(String, String) {
  let map = parse_map(input)

  map
  |> set.fold(from: option.None, with: fn(acc, from) {
    let detected =
      map
      |> set.delete(from)
      |> set.fold(from: set.new(), with: fn(acc, asteroid) {
        acc
        |> set.insert(normalise_vector(from:, to: asteroid))
      })
      |> set.size

    acc
    |> option.unwrap(or: detected)
    |> fn(prev) {
      case detected > prev {
        True -> detected
        False -> prev
      }
    }
    |> option.Some
  })
  |> option.map(int.to_string)
  |> option.to_result("No asteroids found")
}

pub fn part2(input: String) -> Result(String, String) {
  Ok("TODO")
}

type Position {
  Position(x: Int, y: Int)
}

fn parse_map(input: String) -> set.Set(Position) {
  let rows =
    input
    |> string.split(on: "\n")
    |> list.map(string.split(_, on: ""))

  use acc, row, y <- list.index_fold(over: rows, from: set.new())
  use acc, at, x <- list.index_fold(over: row, from: acc)

  case at {
    "#" -> acc |> set.insert(Position(x:, y:))
    _ -> acc
  }
}

fn normalise_vector(from from: Position, to to: Position) -> Position {
  let dx = to.x - from.x
  let dy = to.y - from.y
  let divisor = int.absolute_value(gcd(dx, dy))

  Position(x: dx / divisor, y: dy / divisor)
}

fn gcd(a: Int, b: Int) -> Int {
  case b {
    0 -> a
    _ -> gcd(b, a % b)
  }
}
