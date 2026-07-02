import gleam/dict
import gleam/int
import gleam/list
import gleam/order
import gleam/result
import gleam/string
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

  use tiles <- result.try(
    free_game
    |> intcode.with_input([0])
    |> intcode.run
    |> result.map_error(intcode.error_to_string)
    |> result.try(fn(computer) {
      computer
      |> intcode.output
      |> output_to_tiles
    }),
  )

  tiles
  |> list.group(by: fn(tile) {
    case tile {
      Score(_) -> -1
      Tile(_, Position(x:, y:)) -> y
    }
  })
  |> dict.to_list
  |> list.each(fn(row) { echo row })

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

type Output {
  Tile(Tile, Position)
  Score(Int)
}

fn output_to_tiles(output: List(Int)) -> Result(List(Output), String) {
  output
  |> list.sized_chunk(into: 3)
  |> list.try_map(fn(chunk) {
    echo chunk
    case chunk {
      [-1, 0, score] -> Ok(Score(score))
      [x, y, tile_id] -> {
        let position = Position(x:, y:)
        case tile_id {
          0 -> Ok(Tile(Empty, position))
          1 -> Ok(Tile(Wall, position))
          2 -> Ok(Tile(Block, position))
          3 -> Ok(Tile(Paddle, position))
          4 -> Ok(Tile(Ball, position))
          _ -> Error("Invalid tile id [" <> int.to_string(tile_id) <> "]")
        }
      }
      _ ->
        Error(
          "Invalid output chunk length ["
          <> chunk |> list.length |> int.to_string
          <> "]",
        )
    }
  })
}
