import gleam/dict
import gleam/int
import gleam/list
import gleam/option
import gleam/order
import gleam/pair
import gleam/result
import gleam/set
import gleam/string
import gleam_community/maths

pub const part1_answer = 247

pub const part2_answer = 1919

pub fn part1(input: String) -> Result(String, String) {
  input
  |> parse_map
  |> find_best_location_for_monitoring_station
  |> result.map(pair.second)
  |> result.map(int.to_string)
}

pub fn part2(input: String) -> Result(String, String) {
  let map = parse_map(input)

  use best_location <- result.try(
    map
    |> find_best_location_for_monitoring_station
    |> result.map(pair.first),
  )

  map
  |> vaporize(200, from: best_location)
  |> result.map(fn(location) { location.x * 100 + location.y })
  |> result.map(int.to_string)
}

type Location {
  Location(x: Int, y: Int)
}

type Direction {
  Direction(x: Int, y: Int)
}

type DirectionToAsteroid {
  DirectionToAsteroid(direction: Direction, to: Location, distance: Int)
}

fn parse_map(input: String) -> set.Set(Location) {
  let rows =
    input
    |> string.split(on: "\n")
    |> list.map(string.split(_, on: ""))

  use acc, row, y <- list.index_fold(over: rows, from: set.new())
  use acc, at, x <- list.index_fold(over: row, from: acc)

  case at {
    "#" -> acc |> set.insert(Location(x:, y:))
    _ -> acc
  }
}

fn find_best_location_for_monitoring_station(
  map: set.Set(Location),
) -> Result(#(Location, Int), String) {
  map
  |> set.fold(from: option.None, with: fn(acc, from) {
    let detected =
      map
      |> set.delete(from)
      |> set.fold(from: set.new(), with: fn(acc, asteroid) {
        acc
        |> set.insert(get_direction_to_asteroid(from:, to: asteroid).direction)
      })
      |> set.size

    acc
    |> option.unwrap(or: #(from, detected))
    |> fn(prev) {
      case detected > prev.1 {
        True -> #(from, detected)
        False -> prev
      }
    }
    |> option.Some
  })
  |> option.to_result("No asteroids found")
}

fn vaporize(
  map: set.Set(Location),
  until: Int,
  from best_location: Location,
) -> Result(Location, String) {
  map
  |> set.delete(best_location)
  |> set.fold(from: dict.new(), with: fn(acc, asteroid) {
    let direction_to_asteroid =
      get_direction_to_asteroid(from: best_location, to: asteroid)

    acc
    |> dict.upsert(update: direction_to_asteroid.direction, with: fn(asteroids) {
      case asteroids {
        option.None -> [direction_to_asteroid]
        option.Some(a) ->
          [direction_to_asteroid, ..a]
          |> list.sort(by: fn(a, b) {
            case a.distance < b.distance {
              True -> order.Lt
              False -> order.Gt
            }
          })
      }
    })
  })
  |> dict.to_list
  |> list.map(fn(entry) {
    let #(key, value) = entry
    #(maths.atan2(int.to_float(key.x), int.to_float(key.y)), value)
  })
  |> list.sort(by: fn(a, b) {
    case a.0 <. b.0 {
      True -> order.Gt
      False -> order.Lt
    }
  })
  |> list.map(pair.second)
  |> list.interleave
  |> list.fold_until(
    from: #(0, option.None),
    with: fn(acc, direction_to_asteroid) {
      case acc.0 == until {
        True -> list.Stop(acc)
        False ->
          list.Continue(#(acc.0 + 1, option.Some(direction_to_asteroid.to)))
      }
    },
  )
  |> pair.second
  |> option.to_result("No asteroids vaporized")
}

fn get_direction_to_asteroid(
  from from: Location,
  to to: Location,
) -> DirectionToAsteroid {
  let dx = to.x - from.x
  let dy = to.y - from.y
  let divisor = int.absolute_value(maths.gcd(dx, dy))

  let distance = int.absolute_value(dx + dy)

  DirectionToAsteroid(
    direction: Direction(x: dx / divisor, y: dy / divisor),
    to:,
    distance:,
  )
}
