import gleam/int
import gleam/list
import gleam/result
import gleam/string
import lib/intcode

pub const part1_answer = 3_512_778_005

pub fn part1(input: String) -> Result(String, String) {
  use boost_program <- result.try(
    intcode.parse_program(input) |> result.map_error(intcode.error_to_string),
  )

  boost_program
  |> intcode.boot
  |> intcode.with_input([1])
  |> intcode.run
  |> result.map_error(intcode.error_to_string)
  |> result.try(fn(computer) {
    case computer |> intcode.output {
      [boost_keycode] -> Ok(boost_keycode |> int.to_string)
      errors ->
        Error(errors |> list.map(int.to_string) |> string.join(with: ","))
    }
  })
}

pub fn part2(input: String) -> Result(String, String) {
  Ok("TODO")
}
