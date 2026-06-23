import gleam/dict
import gleam/int
import gleam/list
import gleam/option
import gleam/pair
import gleam/result
import gleam/string

pub const part1_answer = 731

pub const part2_answer = 5672

pub fn part1(input: String) -> Result(String, String) {
  use wires <- result.try(parse_wires(input))

  wires
  |> pair.map_first(to_steps)
  |> pair.map_second(to_steps)
  |> steps_to_intersections
  |> list.map(distance_from_central_port)
  |> list.reduce(int.min)
  |> result.replace_error("No wire intersections found")
  |> result.map(int.to_string)
}

pub fn part2(input: String) -> Result(String, String) {
  use wires <- result.try(parse_wires(input))

  let steps =
    wires
    |> pair.map_first(to_steps)
    |> pair.map_second(to_steps)

  use step_counts <- result.try({
    let #(first_steps, second_steps) = steps

    use intersection <- list.try_map(steps_to_intersections(steps))

    use steps_along_first <- result.try(
      dict.get(first_steps, intersection)
      |> result.replace_error(
        "First wire steps not found @ " <> position_to_string(intersection),
      ),
    )

    use steps_along_second <- result.map(
      dict.get(second_steps, intersection)
      |> result.replace_error(
        "Second wire steps not found @ " <> position_to_string(intersection),
      ),
    )

    steps_along_first + steps_along_second
  })

  step_counts
  |> list.reduce(int.min)
  |> result.replace_error("No wire intersections found")
  |> result.map(int.to_string)
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

fn position_to_string(pos: Position) -> String {
  "(" <> int.to_string(pos.x) <> ", " <> int.to_string(pos.y) <> ")"
}

type Steps =
  dict.Dict(Position, Int)

const central_port = Position(x: 0, y: 0)

fn parse_wires(input: String) -> Result(#(Wire, Wire), String) {
  use #(first, second) <- result.try(
    input
    |> string.trim
    |> string.split_once(on: "\n")
    |> result.replace_error("Did not find segments for exactly 2 wires"),
  )

  use first <- result.try(parse_wire(first))
  use second <- result.map(parse_wire(second))

  #(first, second)
}

fn parse_wire(input: String) -> Result(Wire, String) {
  use segments <- result.map(
    input
    |> string.split(on: ",")
    |> list.try_map(parse_segment),
  )

  Wire(segments:)
}

fn parse_segment(dir: String) -> Result(Segment, String) {
  use #(direction, distance) <- result.try(
    string.pop_grapheme(dir)
    |> result.replace_error("Invalid direction [" <> dir <> "]"),
  )

  use direction <- result.try(case direction {
    "U" -> Ok(Up)
    "R" -> Ok(Right)
    "D" -> Ok(Down)
    "L" -> Ok(Left)
    _ -> Error("Invalid direction [" <> dir <> "]")
  })

  use distance <- result.map(
    int.parse(distance)
    |> result.replace_error("Invalid distance [" <> distance <> "]"),
  )

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

fn steps_to_intersections(steps: #(Steps, Steps)) -> List(Position) {
  steps
  |> pair.first
  |> dict.filter(fn(position, _) { dict.has_key(pair.second(steps), position) })
  |> dict.keys
}

fn distance_from_central_port(position: Position) -> Int {
  int.absolute_value(position.x - central_port.x)
  + int.absolute_value(position.y - central_port.y)
}
