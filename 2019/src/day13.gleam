import gleam/int
import gleam/list
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
  Ok("TODO")
}
