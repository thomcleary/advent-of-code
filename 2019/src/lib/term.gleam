import gleam/int
import gleam/list
import gleam/string

@external(erlang, "io", "columns")
pub fn columns() -> Result(Int, Nil)

pub type Output {
  Text(String)
  Escape(List(Code))
  Reset
}

pub fn output_to_string(parts: List(Output)) -> String {
  list.map(parts, fn(part) {
    case part {
      Text(s) -> s
      Escape(codes) -> list.map(codes, code_to_string) |> string.join(with: "")
      Reset -> "\u{1B}[0m"
    }
  })
  |> string.join("")
}

pub fn output_length(parts: List(Output)) -> Int {
  parts
  |> list.fold(from: 0, with: fn(length, output) {
    case output {
      Text(t) -> t |> string.replace(each: "\n", with: "") |> string.length
      _ -> 0
    }
    + length
  })
}

pub type Box {
  Box(
    horizontal: String,
    vertical: String,
    vertical_and_right: String,
    arc_up_and_left: String,
    arc_up_and_right: String,
    arc_down_and_left: String,
    arc_down_and_right: String,
  )
}

pub type BoxCode {
  BoxCode(light: Box)
}

pub const box = BoxCode(
  light: Box(
    horizontal: "\u{2500}",
    vertical: "\u{2502}",
    vertical_and_right: "\u{251C}",
    arc_down_and_right: "\u{256D}",
    arc_down_and_left: "\u{256E}",
    arc_up_and_left: "\u{256F}",
    arc_up_and_right: "\u{2570}",
  ),
)

pub type Block {
  Block(
    upper_half: String,
    lower_half: String,
    left_half: String,
    left_three_quarters: String,
    right_half: String,
    quadrant_lower_left: String,
    quadrant_lower_right: String,
    quadrant_upper_left: String,
    quadrant_upper_right: String,
  )
}

pub const block = Block(
  upper_half: "\u{2580}",
  lower_half: "\u{2584}",
  left_half: "\u{258C}",
  left_three_quarters: "\u{258A}",
  right_half: "\u{2590}",
  quadrant_lower_left: "\u{2596}",
  quadrant_lower_right: "\u{2597}",
  quadrant_upper_left: "\u{2598}",
  quadrant_upper_right: "\u{259D}",
)

pub type Code {
  Bold
  Dim
  Italic
  Underline
  SlowBlink
  RapidBlink
  Strike
  FgBlack
  FgRed
  FgGreen
  FgYellow
  FgBlue
  FgMagenta
  FgCyan
  FgWhite
  BgBlack
  BgRed
  BgGreen
  BgYellow
  BgBlue
  BgMagenta
  BgCyan
  BgWhite
  Overline
  FgBrightBlack
  FgBrightRed
  FgBrightGreen
  FgBrightYellow
  FgBrightBlue
  FgBrightMagenta
  FgBrightCyan
  FgBrightWhite
  BgBrightBlack
  BgBrightRed
  BgBrightGreen
  BgBrightYellow
  BgBrightBlue
  BgBrightMagenta
  BgBrightCyan
  BgBrightWhite
}

fn code_to_string(code: Code) -> String {
  let value =
    {
      case code {
        Bold -> 1
        Dim -> 2
        Italic -> 3
        Underline -> 4
        SlowBlink -> 5
        RapidBlink -> 6
        Strike -> 9
        FgBlack -> 30
        FgRed -> 31
        FgGreen -> 32
        FgYellow -> 33
        FgBlue -> 34
        FgMagenta -> 35
        FgCyan -> 36
        FgWhite -> 37
        BgBlack -> 40
        BgRed -> 41
        BgGreen -> 42
        BgYellow -> 43
        BgBlue -> 44
        BgMagenta -> 45
        BgCyan -> 46
        BgWhite -> 47
        Overline -> 53
        FgBrightBlack -> 90
        FgBrightRed -> 91
        FgBrightGreen -> 92
        FgBrightYellow -> 93
        FgBrightBlue -> 94
        FgBrightMagenta -> 95
        FgBrightCyan -> 96
        FgBrightWhite -> 97
        BgBrightBlack -> 100
        BgBrightRed -> 101
        BgBrightGreen -> 102
        BgBrightYellow -> 103
        BgBrightBlue -> 104
        BgBrightMagenta -> 105
        BgBrightCyan -> 106
        BgBrightWhite -> 107
      }
    }
    |> int.to_string

  "\u{1B}[" <> value <> "m"
}
