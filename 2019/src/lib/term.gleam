import gleam/int
import gleam/string

pub type Box {
  Box(
    horizontal: String,
    vertical: String,
    down_and_left: String,
    up_and_right: String,
    down_and_right: String,
  )
}

pub type BoxDrawing {
  BoxDrawing(light: Box)
}

pub const box = BoxDrawing(
  light: Box(
    horizontal: "\u{2500}",
    vertical: "\u{2502}",
    down_and_left: "\u{2510}",
    up_and_right: "\u{2514}",
    down_and_right: "\u{250C}",
  ),
)

@external(erlang, "term_ffi", "columns")
pub fn columns() -> Result(Int, Nil)

pub fn escape(str: String, code: EscapeCode) -> String {
  esc
  |> string.append(code |> to_string)
  |> string.append(str)
  |> string.append(reset)
}

const esc = "\u{1B}["

const reset = "\u{1B}[0m"

pub type EscapeCode {
  Bold
  Faint
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

pub fn to_string(code: EscapeCode) -> String {
  {
    case code {
      Bold -> 1
      Faint -> 2
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
  |> string.append("m")
}
