import gleam/dict
import gleam/int
import gleam/list
import gleam/order
import gleam/pair
import gleam/result
import gleam/string
import lib/intcode
import lib/term

pub const part1_answer = 1885

pub fn part1(input: String) -> Result(String, String) {
  input
  |> intcode.parse_program
  |> result.map_error(intcode.error_to_string)
  |> result.try(paint_panels(_, starting_on: Black))
  |> result.map(fn(panels) {
    panels
    |> dict.keys
    |> list.length
    |> int.to_string
  })
}

pub fn part2(input: String) -> Result(String, String) {
  input
  |> intcode.parse_program
  |> result.map_error(intcode.error_to_string)
  |> result.try(paint_panels(_, starting_on: White))
  |> result.try(panels_to_string)
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
  starting_on start_colour: Colour,
) -> Result(dict.Dict(Position, Colour), String) {
  let computer = program |> intcode.boot |> intcode.with_blocking_io
  let robot = Robot(position: Position(x: 0, y: 0), direction: Up)
  let panels = dict.new() |> dict.insert(robot.position, start_colour)

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
      case computer |> intcode.output {
        [colour_output, turn_output] -> {
          use colour <- result.try(parse_colour(colour_output))
          use next_robot <- result.try(get_next_robot(robot, turn_output))

          do_paint_panels(
            computer: computer |> intcode.purge_output,
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

fn get_next_robot(robot: Robot, turn: Int) -> Result(Robot, String) {
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
    Up -> Position(x: robot.position.x, y: robot.position.y - 1)
    Right -> Position(x: robot.position.x + 1, y: robot.position.y)
    Down -> Position(x: robot.position.x, y: robot.position.y + 1)
    Left -> Position(x: robot.position.x - 1, y: robot.position.y)
  }

  Robot(position:, direction:)
}

fn panels_to_string(
  panels: dict.Dict(Position, Colour),
) -> Result(String, String) {
  let x_positions = panels |> dict.keys |> list.map(fn(p) { p.x })

  use x_min <- result.try(
    x_positions
    |> list.max(order.reverse(int.compare))
    |> result.replace_error("failed finding x_min"),
  )
  use x_max <- result.map(
    x_positions
    |> list.max(int.compare)
    |> result.replace_error("failed finding x_max"),
  )

  let column_range =
    int.range(from: x_min, to: x_max + 1, with: [], run: list.prepend)

  let panels_by_row =
    panels
    |> dict.to_list
    |> list.group(by: fn(panel) {
      {
        panel
        |> pair.first
      }.y
    })
    |> dict.to_list
    |> list.sort(by: fn(a, b) {
      case a.0 < b.0 {
        True -> order.Lt
        False -> order.Gt
      }
    })

  let rows =
    panels_by_row
    |> list.map(with: fn(row) {
      row
      |> pair.map_second(with: fn(positions) {
        let columns =
          positions
          |> list.fold(from: dict.new(), with: fn(acc, position) {
            acc
            |> dict.insert(position.0.x, position.1)
          })

        [
          term.Text("  "),
          ..column_range
          |> list.fold(from: [], with: fn(acc, c) {
            [
              [
                term.Reset,
                term.Escape([
                  case dict.get(columns, c) {
                    Ok(Black) | Error(_) -> term.BgBlack
                    Ok(White) -> term.BgWhite
                  },
                ]),
                term.Text(" "),
                term.Reset,
              ],
              ..acc
            ]
          })
          |> list.flatten
        ]
      })
    })
    |> list.map(pair.second)
    |> list.map(term.output_to_string)
    |> string.join(with: "\n")

  "\n\n" <> rows <> "\n"
}
