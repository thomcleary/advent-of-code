import gleam/int
import gleam/list
import gleam/order
import gleam/result
import gleam/string
import lib/term

pub const part1_answer = 2193

pub fn part1(input: String) -> Result(String, String) {
  use image_data <- result.try(parse_image_data(input))

  image_data
  |> data_to_layers
  |> list.max(with: fn(a, b) {
    let pred = fn(p) { p == Black }

    case list.count(a, pred) < list.count(b, pred) {
      True -> order.Gt
      False -> order.Lt
    }
  })
  |> result.replace_error("No image layers")
  |> result.map(fn(layer) {
    list.count(layer, fn(p) { p == White })
    * list.count(layer, fn(p) { p == Transparent })
    |> int.to_string
  })
}

pub fn part2(input: String) -> Result(String, String) {
  use image_data <- result.try(parse_image_data(input))

  use #(first_layer, rest_layers) <- result.map(
    case image_data |> data_to_layers {
      [first, ..rest] -> Ok(#(first, rest))
      _ -> Error("No image layers")
    },
  )

  rest_layers
  |> list.fold(from: first_layer, with: fn(image, layer) {
    list.map2(image, layer, with: fn(front_pixel, back_pixel) {
      case front_pixel {
        Transparent -> back_pixel
        _ -> front_pixel
      }
    })
  })
  |> list.sized_chunk(into: width)
  |> image_to_string
}

const width = 25

const height = 6

type Pixel {
  Black
  White
  Transparent
}

type ImageData =
  List(Pixel)

type ImageLayers =
  List(ImageData)

fn parse_image_data(input: String) -> Result(ImageData, String) {
  input
  |> string.split(on: "")
  |> list.try_map(int.parse)
  |> result.try(
    list.try_map(_, fn(p) {
      case p {
        0 -> Ok(Black)
        1 -> Ok(White)
        2 -> Ok(Transparent)
        _ -> Error(Nil)
      }
    }),
  )
  |> result.replace_error("Invalid image data")
}

fn data_to_layers(data: ImageData) -> ImageLayers {
  data
  |> list.sized_chunk(into: width * height)
}

fn image_to_string(data: List(ImageData)) -> String {
  let image =
    data
    |> list.map(fn(pixels) {
      [
        [term.Text("  ")],
        pixels
          |> list.map(fn(pixel) {
            [
              term.Reset,
              term.Escape(case pixel {
                Black -> [term.BgBlack]
                White -> [term.BgWhite]
                Transparent -> []
              }),
              term.Text(" "),
              term.Reset,
            ]
          })
          |> list.flatten,
      ]
      |> list.flatten
    })
    |> list.intersperse([term.Text("\n")])
    |> list.flatten
    |> term.output_to_string

  "\n\n" <> image <> "\n"
}
