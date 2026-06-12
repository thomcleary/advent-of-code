import gleam/io
import gleam/result
import gleam/string
import lib/term

pub type Part {
  One
  Two
}

pub const to_do = "TODO"

pub fn print(answer: String, part: Part) -> Nil {
  answer
  |> do(part)
}

pub fn try_print(answer: Result(String, Nil), part: Part) -> Nil {
  answer
  |> result.unwrap(or: "Error" |> term.escape(term.FgRed))
  |> do(part)
}

fn do(answer: String, part: Part) -> Nil {
  let part_number = case part {
    One -> "1"
    Two -> "2"
  }

  term.box.light.vertical
  |> term.escape(term.FgMagenta)
  |> string.append(" ")
  |> string.append(
    "Part"
    |> string.append(" ")
    |> string.append(part_number)
    |> string.append(":")
    |> term.escape(term.Faint)
    |> string.append(" ")
    |> string.append(
      case answer {
        a if a == to_do -> answer |> term.escape(term.FgYellow)
        _ -> answer
      }
      |> term.escape(term.Bold),
    ),
  )
  |> io.println
}
