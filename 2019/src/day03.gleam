import gleam/dict
import gleam/int
import gleam/list
import gleam/option
import gleam/pair
import gleam/result
import gleam/string
import lib/part

pub fn solve(input: String) -> Nil {
  let assert Ok(wires) = parse_wires(input)

  wires
  |> part1
  |> result.map(int.to_string)
  |> part.try_print(part.One)

  wires
  |> part2
  |> result.map(int.to_string)
  |> part.try_print(part.Two)
}

pub fn part1(wires: #(Wire, Wire)) -> Result(Int, Nil) {
  wires
  |> pair.map_first(to_steps)
  |> pair.map_second(to_steps)
  |> intersections
  |> list.map(distance_from_central_port)
  |> list.reduce(int.min)
}

pub fn part2(wires: #(Wire, Wire)) -> Result(Int, Nil) {
  let steps =
    wires
    |> pair.map_first(to_steps)
    |> pair.map_second(to_steps)

  steps
  |> intersections
  |> list.try_map(fn(position) {
    use first <- result.try(pair.first(steps) |> dict.get(position))
    use second <- result.map(pair.second(steps) |> dict.get(position))
    first + second
  })
  |> result.try(list.reduce(_, int.min))
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

type Steps =
  dict.Dict(Position, Int)

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
  use segments <- result.map(
    input
    |> string.split(on: ",")
    |> list.try_map(parse_segment),
  )

  Wire(segments:)
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

  use distance <- result.map(int.parse(distance))

  Segment(distance:, direction:)
}

type State {
  State(position: Position, steps_taken: Int, steps: dict.Dict(Position, Int))
}

fn to_steps(wire: Wire) -> Steps {
  list.fold(
    over: wire.segments,
    from: State(position: central_port, steps_taken: 0, steps: dict.new()),
    with: step,
  ).steps
  |> dict.delete(central_port)
}

fn step(state: State, segment: Segment) -> State {
  let #(from, to) = case segment.direction {
    Up -> #(state.position.y, state.position.y + segment.distance)
    Right -> #(state.position.x, state.position.x + segment.distance)
    Down -> #(state.position.y, state.position.y - segment.distance)
    Left -> #(state.position.x, state.position.x - segment.distance)
  }

  let take_step = fn(steps, i) {
    let position = state.position |> move(segment.direction, to: i)
    let steps_taken = int.absolute_value(i - from) + state.steps_taken

    steps
    |> dict.upsert(update: position, with: option.unwrap(_, or: steps_taken))
  }

  State(
    position: state.position |> move(segment.direction, to:),
    steps_taken: state.steps_taken + segment.distance,
    steps: int.range(from:, to:, with: state.steps, run: take_step),
  )
}

fn move(position: Position, direction: Direction, to to: Int) {
  case direction {
    Up | Down -> Position(..position, y: to)
    Right | Left -> Position(..position, x: to)
  }
}

fn intersections(steps: #(Steps, Steps)) -> List(Position) {
  steps
  |> pair.first
  |> dict.filter(fn(position, _) { dict.has_key(pair.second(steps), position) })
  |> dict.keys
}

fn distance_from_central_port(position: Position) -> Int {
  int.absolute_value(position.x - central_port.x)
  + int.absolute_value(position.y - central_port.y)
}
