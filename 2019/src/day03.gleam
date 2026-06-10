import gleam/int
import gleam/io
import gleam/list
import gleam/pair
import gleam/result
import gleam/set
import gleam/string

pub fn solve(input: String) -> Nil {
  let assert Ok(wires) = parse_wires(input)

  let assert Ok(part1_answer) = part1(wires)
  io.println("Part 1: " <> int.to_string(part1_answer))

  let assert Ok(part2_answer) = part2(wires)
  io.println("Part 2: " <> int.to_string(part2_answer))
}

pub fn part1(wires: #(Wire, Wire)) -> Result(Int, Nil) {
  let positions =
    wires
    |> pair.map_first(wire_to_positions)
    |> pair.map_second(wire_to_positions)

  use distance_to_closest_intersection <- result.try(
    set.intersection(pair.first(positions), pair.second(positions))
    |> set.to_list
    |> list.map(distance_from_central_port)
    |> list.reduce(int.min),
  )

  Ok(distance_to_closest_intersection)
}

pub fn part2(wires: #(Wire, Wire)) -> Result(Int, Nil) {
  // Refactor the solution, so that instead of creating sets,
  // create a Dict(key: Position, value: Int) 
  // where value is # of steps to Position

  // For part1, this will just mean getting the intersection
  // of both wires dict keys

  // For part2
  // - get the intersection of the keys
  // - map each key, to the sum of its values from each dict
  // - return the minimum

  todo
}

pub type Direction {
  Up
  Right
  Down
  Left
}

pub type Segment {
  Segment(direction: Direction, distance: Int)
}

pub type Wire {
  Wire(segments: List(Segment))
}

type Position {
  Position(x: Int, y: Int)
}

const central_port = Position(x: 0, y: 0)

fn parse_wires(input: String) -> Result(#(Wire, Wire), Nil) {
  use #(first, second) <- result.try(
    input
    |> string.trim
    |> string.split_once(on: "\n"),
  )

  use first <- result.try(parse_wire(first))
  use second <- result.try(parse_wire(second))

  Ok(#(first, second))
}

fn parse_wire(input: String) -> Result(Wire, Nil) {
  use segments <- result.try(
    input
    |> string.split(on: ",")
    |> list.try_map(parse_segment),
  )

  Ok(Wire(segments:))
}

fn parse_segment(dir: String) -> Result(Segment, Nil) {
  use #(direction, distance) <- result.try(string.pop_grapheme(dir))

  use direction <- result.try(case direction {
    "U" -> Ok(Up)
    "R" -> Ok(Right)
    "D" -> Ok(Down)
    "L" -> Ok(Left)
    _ -> Error(Nil)
  })

  use distance <- result.try(int.parse(distance))

  Ok(Segment(distance:, direction:))
}

fn wire_to_positions(wire: Wire) -> set.Set(Position) {
  wire.segments
  |> list.map_fold(from: central_port, with: fn(current_position, segment) {
    let from = case segment.direction {
      Up -> current_position.y
      Right -> current_position.x
      Down -> current_position.y
      Left -> current_position.x
    }

    let to = case segment.direction {
      Up | Right -> from + segment.distance
      Down | Left -> from - segment.distance
    }

    let positions =
      int.range(from:, to:, with: set.new(), run: fn(positions, i) {
        set.insert(positions, case segment.direction {
          Up | Down -> Position(..current_position, y: i)
          Right | Left -> Position(..current_position, x: i)
        })
      })

    let next_position = case segment.direction {
      Up | Down -> Position(..current_position, y: to)
      Right | Left -> Position(..current_position, x: to)
    }

    #(next_position, positions)
  })
  |> pair.second
  |> list.fold(set.new(), set.union)
  |> set.delete(central_port)
}

fn distance_from_central_port(position: Position) -> Int {
  int.absolute_value(position.x + central_port.x)
  + int.absolute_value(position.y + central_port.y)
}
