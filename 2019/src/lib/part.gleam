import gleam/int
import gleam/io
import gleam/result
import gleam/string
import lib/term

pub type Part {
  One
  Two
}

pub fn print(answer: Int, part: Part) -> Nil {
  answer
  |> int.to_string
  |> do(part)
}

pub fn try_print(answer: Result(Int, Nil), part: Part) -> Nil {
  answer
  |> result.map(int.to_string)
  |> result.unwrap(or: "Error" |> term.escape(term.FgRed))
  |> do(part)
}

fn do(answer: String, part: Part) -> Nil {
  let part_number = case part {
    One -> "1"
    Two -> "2"
  }

  "\u{2502}"
  |> term.escape(term.FgMagenta)
  |> string.append(" ")
  |> string.append(
    "Part"
    |> string.append(" ")
    |> string.append(part_number)
    |> string.append(":")
    |> term.escape(term.Faint)
    |> string.append(" ")
    |> string.append(answer |> term.escape(term.Bold)),
  )
  |> io.println
}
