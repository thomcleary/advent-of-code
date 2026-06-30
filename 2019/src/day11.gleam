import gleam/dict
import gleam/int
import gleam/list
import gleam/result
import lib/intcode

pub const part1_answer = 1885

pub fn part1(input: String) -> Result(String, String) {
  use program <- result.try(
    intcode.parse_program(input) |> result.map_error(intcode.error_to_string),
  )

  paint_panels(with: program)
  |> result.map(fn(panels) {
    panels
    |> dict.keys
    |> list.length
    |> int.to_string
  })
}

pub fn part2(input: String) -> Result(String, String) {
  Ok("TODO")
}

type Position {
  Position(x: Int, y: Int)
}

type Direction {
  Up
  Right
  Left
  Down
}

type Robot {
  Robot(position: Position, direction: Direction)
}

type Colour {
  Black
  White
}

fn paint_panels(
  with program: intcode.Program,
) -> Result(dict.Dict(Position, Colour), String) {
  let computer = program |> intcode.boot |> intcode.with_blocking_io
  let robot = Robot(position: Position(x: 0, y: 0), direction: Up)
  let panels = dict.new() |> dict.insert(robot.position, Black)

  do_paint_panels(computer:, robot:, panels:)
}

fn do_paint_panels(
  computer computer: intcode.Computer,
  robot robot: Robot,
  panels panels: dict.Dict(Position, Colour),
) -> Result(dict.Dict(Position, Colour), String) {
  use computer <- result.try(
    computer
    |> intcode.with_input([
      case dict.get(panels, robot.position) |> result.unwrap(or: Black) {
        Black -> 0
        White -> 1
      },
    ])
    |> intcode.run
    |> result.map_error(intcode.error_to_string),
  )

  case computer |> intcode.state {
    intcode.Booted -> Error("computer in invalid state [Booted]")
    intcode.Halted -> Ok(panels)
    intcode.Blocked -> {
      case computer |> intcode.output |> list.reverse {
        [turn_output, colour_output, ..] -> {
          use colour <- result.try(parse_colour(colour_output))
          use next_robot <- result.try(next_robot(robot, turn_output))

          do_paint_panels(
            computer:,
            robot: next_robot,
            panels: panels |> dict.insert(robot.position, colour),
          )
        }
        _ -> Error("invalid output")
      }
    }
  }
}

fn parse_colour(output: Int) -> Result(Colour, String) {
  case output {
    0 -> Ok(Black)
    1 -> Ok(White)
    _ -> Error("Invalid colour [" <> int.to_string(output) <> "]")
  }
}

fn next_robot(robot: Robot, turn: Int) -> Result(Robot, String) {
  use direction <- result.map(case turn {
    0 ->
      Ok(case robot.direction {
        Up -> Left
        Right -> Up
        Down -> Right
        Left -> Down
      })
    1 ->
      Ok(case robot.direction {
        Up -> Right
        Right -> Down
        Down -> Left
        Left -> Up
      })
    _ -> Error("Invalid turn [" <> int.to_string(turn) <> "]")
  })

  let position = case direction {
    Up -> Position(x: robot.position.x, y: robot.position.y + 1)
    Right -> Position(x: robot.position.x + 1, y: robot.position.y)
    Down -> Position(x: robot.position.x, y: robot.position.y - 1)
    Left -> Position(x: robot.position.x - 1, y: robot.position.y)
  }

  Robot(position:, direction:)
}
