import gleam/dict
import gleam/int
import gleam/list
import gleam/pair
import gleam/result
import lib/intcode

pub const part1_answer = 291

pub fn part1(input: String) -> Result(String, String) {
  use game <- result.try(
    intcode.parse_program(input) |> result.map_error(intcode.error_to_string),
  )

  use halted <- result.map(
    game
    |> intcode.boot
    |> intcode.run
    |> result.map_error(intcode.error_to_string),
  )

  halted
  |> intcode.output
  |> list.sized_chunk(into: 3)
  |> list.fold(from: 0, with: fn(block_tiles, output_chunk) {
    case output_chunk {
      [_, _, 2] -> block_tiles + 1
      _ -> block_tiles
    }
  })
  |> int.to_string
}

pub fn part2(input: String) -> Result(String, String) {
  use game <- result.try(
    intcode.parse_program(input) |> result.map_error(intcode.error_to_string),
  )

  use free_game <- result.try(
    game
    |> intcode.boot
    |> intcode.with_blocking_io
    |> intcode.poke_memory(at: intcode.Address(0), with: 2)
    |> result.map_error(intcode.error_to_string),
  )

  use game_state <- result.try(
    free_game
    |> intcode.with_input([0])
    |> intcode.run
    |> result.map_error(intcode.error_to_string)
    |> result.try(fn(computer) {
      computer
      |> intcode.output
      |> output_to_state
    }),
  )

  // TODO
  // group board by position.y (rows)
  // sort rows from smallest to largest
  // sort each row by position.x (columns)
  // map each column from tile -> string
  // join each row's columns by ""
  // join each row by "\n"
  // add score to bottom
  // print out state
  // ask for next input
  // loop
  echo game_state.board
    |> dict.to_list
    |> list.filter(fn(entry) {
      case entry |> pair.second {
        Ball -> True
        _ -> False
      }
    })

  Ok("TODO")
}

type Position {
  Position(x: Int, y: Int)
}

type Tile {
  Empty
  Wall
  Block
  Paddle
  Ball
}

type GameState {
  GameState(score: Int, board: dict.Dict(Position, Tile))
}

fn output_to_state(output: List(Int)) -> Result(GameState, String) {
  output
  |> list.sized_chunk(into: 3)
  |> list.try_fold(
    from: GameState(score: -1, board: dict.new()),
    with: fn(state, chunk) {
      case chunk {
        [-1, 0, score] -> Ok(GameState(..state, score:))
        [x, y, tile_id] -> {
          let position = Position(x:, y:)

          use tile <- result.map(case tile_id {
            0 -> Ok(Empty)
            1 -> Ok(Wall)
            2 -> Ok(Block)
            3 -> Ok(Paddle)
            4 -> Ok(Ball)
            _ -> Error("Invalid tile id [" <> int.to_string(tile_id) <> "]")
          })

          GameState(
            ..state,
            board: state.board |> dict.insert(for: position, insert: tile),
          )
        }
        _ ->
          Error(
            "Invalid output chunk length ["
            <> chunk |> list.length |> int.to_string
            <> "]",
          )
      }
    },
  )
}
