import argv
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import lib/aoc
import lib/term

pub fn main() {
  case argv.load().arguments {
    [] -> list.each(aoc.solved_days, solve)
    [first, ..] ->
      case aoc.parse_day(first) {
        Ok(day) -> solve(day)
        Error(_) -> usage()
      }
  }
}

fn solve(day: aoc.Day) -> Nil {
  case aoc.run_day(day) {
    Ok(day_run) -> day_run_to_term_output(day_run)
    Error(reason) -> [
      term.Reset,
      term.Escape([term.Bold, term.FgRed]),
      term.Text("Day " <> aoc.day_to_string(day) <> ": " <> reason),
    ]
  }
  |> term.output_to_string
  |> string.append("\n")
  |> io.println
}

fn day_run_to_term_output(run: aoc.DayRun) -> List(term.Output) {
  let width =
    int.clamp(result.unwrap(term.columns(), or: 80) - 8, min: 40, max: 80)

  [
    header(day: run.day, width: width),
    row(
      content: part_to_term_output(part: "1", result: run.part1.result),
      width:,
    ),
    row(
      content: part_to_term_output(part: "2", result: run.part2.result),
      width:,
    ),
    footer(times: #(run.part1.time, run.part2.time), width:),
  ]
  |> list.flatten
}

fn header(day day: aoc.Day, width width: Int) -> List(term.Output) {
  let box_content_left = [
    term.Reset,
    term.Escape([term.FgMagenta]),
    term.Text(term.box.light.arc_down_and_right),
  ]

  let text_content = [
    term.Reset,
    term.Escape([term.Bold, term.BgMagenta, term.FgBlack]),
    term.Text("  Day "),
    term.Text(aoc.day_to_string(day)),
    term.Reset,
    term.Escape([term.BgMagenta]),
    term.Text("  "),
  ]

  let text_content_length = term.output_length(text_content)

  let box_top = [
    term.Reset,
    term.Escape([term.FgMagenta]),
    term.Text(
      " "
      <> string.repeat(term.block.lower_half, times: text_content_length)
      <> " "
      <> "\n",
    ),
  ]

  let box_content_right = [
    term.Reset,
    term.Escape([term.FgMagenta]),
    term.Text(
      term.box.light.horizontal
      <> string.repeat(
        term.box.light.horizontal,
        times: width - term.output_length(box_top) - 1,
      )
      <> term.box.light.arc_down_and_left
      <> "\n",
    ),
  ]

  let box_bottom = [
    term.Reset,
    term.Escape([term.FgMagenta]),
    term.Text(
      term.box.light.vertical
      <> string.repeat(term.block.upper_half, times: text_content_length)
      <> string.repeat(" ", times: term.output_length(box_content_right) - 1)
      <> term.box.light.vertical
      <> "\n",
    ),
  ]

  [box_top, box_content_left, text_content, box_content_right, box_bottom]
  |> list.flatten
}

fn row(
  content content: List(term.Output),
  width width: Int,
) -> List(term.Output) {
  let box_left = [
    term.Reset,
    term.Escape([term.FgMagenta]),
    term.Text(term.box.light.vertical),
  ]

  let box_right = [
    term.Reset,
    term.Escape([term.FgMagenta]),
    term.Text(term.box.light.vertical),
  ]

  let padding = [
    term.Reset,
    term.Text(string.repeat(
      " ",
      times: width
        - term.output_length(box_left)
        - term.output_length(content)
        - term.output_length(box_right),
    )),
  ]

  [box_left, content, padding, box_right, [term.Text("\n")]]
  |> list.flatten
}

fn footer(times times: #(Int, Int), width width: Int) -> List(term.Output) {
  let #(part1_time, part2_time) = times
  let total_time = part1_time + part2_time

  let ms = "ms"

  let text_content = [
    term.Reset,
    term.Escape([term.Dim]),
    term.Text(" "),
    term.Text(int.to_string(part1_time)),
    term.Text(ms <> " + "),
    term.Text(int.to_string(part2_time)),
    term.Text(ms <> " = "),
    term.Reset,
    term.Escape([term.Bold, term.FgYellow]),
    term.Text(int.to_string(total_time)),
    term.Text(ms <> " "),
  ]

  let box_right = [
    term.Reset,
    term.Escape([term.FgMagenta]),
    term.Text(
      string.repeat(term.box.light.horizontal, times: 1)
      <> term.box.light.arc_up_and_left,
    ),
  ]

  let box_left = [
    term.Reset,
    term.Escape([term.FgMagenta]),
    term.Text(
      term.box.light.arc_up_and_right
      <> string.repeat(
        term.box.light.horizontal,
        times: width
          - 1
          - term.output_length(text_content)
          - term.output_length(box_right),
      ),
    ),
  ]

  [box_left, text_content, box_right]
  |> list.flatten
}

fn part_to_term_output(
  part part: String,
  result part_result: Result(aoc.PartAssertion, String),
) -> List(term.Output) {
  [
    term.Reset,
    term.Escape([term.FgYellow]),
    term.Text(" Part " <> part),
    term.Text(": "),
    term.Reset,
    term.Escape({
      case part_result {
        Ok(assertion) ->
          case assertion {
            aoc.Todo(_) -> [term.Dim]
            aoc.Pass(_) -> [term.Bold]
            aoc.Fail(_, _) -> [term.Bold, term.FgRed]
          }
        Error(_) -> [term.Bold, term.FgRed]
      }
    }),
    // TODO: break result text into lines, so it doesn't overflow the box
    // TODO: If part has multiline answer
    // - replace \n with padding + box + \n, so we can draw the right box side
    // - start beginning of the answer on a new line, and prepend each line with left side of box
    term.Text(case part_result {
      Error(reason) -> reason
      Ok(assertion) ->
        case assertion {
          aoc.Todo(value:) -> value
          aoc.Pass(value:) -> value
          aoc.Fail(actual:, expected:) -> actual <> " (" <> expected <> ")"
        }
    }),
  ]
}

fn usage() -> Nil {
  let usage = [
    term.Reset,
    term.Escape([term.FgMagenta]),
    term.Text("usage: "),
  ]

  let command = [
    term.Reset,
    term.Escape([term.Bold]),
    term.Text("gleam run "),
  ]

  let day_arg =
    [
      [term.Reset, term.Escape([term.Dim]), term.Text("[{")],
      list.index_map(aoc.solved_days, fn(day, i) {
        let separator = case i < list.length(aoc.solved_days) - 1 {
          True -> [term.Reset, term.Escape([term.Dim]), term.Text("|")]
          False -> []
        }
        [
          term.Reset,
          term.Escape([term.FgYellow]),
          term.Text(aoc.day_to_string(day)),
          ..separator
        ]
      })
        |> list.flatten,
      [term.Reset, term.Escape([term.Dim]), term.Text("}]")],
    ]
    |> list.flatten

  [[term.Text("\n")], usage, command, day_arg]
  |> list.flatten
  |> term.output_to_string
  |> io.println
}
