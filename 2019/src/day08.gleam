import gleam/int
import gleam/list
import gleam/order
import gleam/result
import gleam/string

pub const part1_answer = 2193

pub fn part1(input: String) -> Result(Int, String) {
  use image_data <- result.try(parse_image_data(input))

  let width = 25
  let height = 6

  image_data
  |> list.sized_chunk(into: width * height)
  |> list.max(with: fn(a, b) {
    let pred = fn(x) { x == 0 }

    case list.count(a, pred) < list.count(b, pred) {
      True -> order.Gt
      False -> order.Lt
    }
  })
  |> result.replace_error("No image layers")
  |> result.map(fn(layer) {
    list.count(layer, fn(x) { x == 1 }) * list.count(layer, fn(x) { x == 2 })
  })
}

pub fn part2(input: String) -> Result(Int, String) {
  Ok(-1)
}

fn parse_image_data(input: String) -> Result(List(Int), String) {
  input
  |> string.split(on: "")
  |> list.try_map(int.parse)
  |> result.replace_error("Image data contains non integer character")
}
