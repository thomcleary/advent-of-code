import gleam/int
import gleam/list
import gleam/option
import gleam/result
import lib/intcode

pub const part1_answer = 92_663

pub const part2_answer = 14_365_052

pub fn part1(input: String) -> Result(String, String) {
  use amplifier_controller_software <- result.try(
    intcode.parse_program(input)
    |> result.map_error(intcode.error_to_string),
  )

  use permutations <- result.try(phase_settings_permutations(from: 0, to: 4))

  amplifier_controller_software
  |> run(over: permutations, with: run_amps)
  |> result.map(int.to_string)
}

pub fn part2(input: String) -> Result(String, String) {
  use amplifier_controller_software <- result.try(
    intcode.parse_program(input)
    |> result.map_error(intcode.error_to_string),
  )

  use permutations <- result.try(phase_settings_permutations(from: 5, to: 9))

  amplifier_controller_software
  |> run(over: permutations, with: run_amps_in_feedback_loop)
  |> result.map(int.to_string)
}

type PhaseSettings =
  #(Int, Int, Int, Int, Int)

fn range_to_phase_settings(list: List(Int)) -> Result(PhaseSettings, String) {
  case list {
    [a, b, c, d, e] -> Ok(#(a, b, c, d, e))
    _ -> Error("Invalid phase settings")
  }
}

fn phase_settings_permutations(
  from from: Int,
  to to: Int,
) -> Result(List(PhaseSettings), String) {
  int.range(from: to, to: from - 1, with: [], run: list.prepend)
  |> list.permutations
  |> list.try_map(range_to_phase_settings)
}

fn run(
  controller_software software: intcode.Program,
  over permutations: List(PhaseSettings),
  with with: fn(intcode.Program, PhaseSettings) -> Result(Int, String),
) -> Result(Int, String) {
  use highest <- result.try({
    use acc, phase_settings <- list.try_fold(
      over: permutations,
      from: option.None,
    )

    use thruster_signal <- result.try(
      software
      |> with(phase_settings),
    )

    acc
    |> option.map(with: int.max(_, thruster_signal))
    |> option.unwrap(or: thruster_signal)
    |> option.Some
    |> Ok
  })

  option.to_result(highest, "Failed to find any thruster signal")
}

fn run_amps(
  program: intcode.Program,
  phase_settings: PhaseSettings,
) -> Result(Int, String) {
  let #(phase_a, phase_b, phase_c, phase_d, phase_e) = phase_settings

  let amp = program |> intcode.boot

  let amp_a = amp |> intcode.with_input([phase_a])
  let amp_b = amp |> intcode.with_input([phase_b])
  let amp_c = amp |> intcode.with_input([phase_c])
  let amp_d = amp |> intcode.with_input([phase_d])
  let amp_e = amp |> intcode.with_input([phase_e])

  use output_a <- result.try(
    amp_a
    |> run_amp(with: 0)
    |> result.try(get_amp_output),
  )

  use output_b <- result.try(
    amp_b
    |> run_amp(with: output_a)
    |> result.try(get_amp_output),
  )

  use output_c <- result.try(
    amp_c
    |> run_amp(with: output_b)
    |> result.try(get_amp_output),
  )

  use output_d <- result.try(
    amp_d
    |> run_amp(with: output_c)
    |> result.try(get_amp_output),
  )

  amp_e
  |> run_amp(with: output_d)
  |> result.try(get_amp_output)
}

type Amplifiers {
  Amplifiers(
    a: intcode.Computer,
    b: intcode.Computer,
    c: intcode.Computer,
    d: intcode.Computer,
    e: intcode.Computer,
  )
}

fn run_amps_in_feedback_loop(
  program: intcode.Program,
  phase_settings: PhaseSettings,
) -> Result(Int, String) {
  let #(phase_a, phase_b, phase_c, phase_d, phase_e) = phase_settings

  let feedback_amp = program |> intcode.boot |> intcode.with_blocking_io

  Amplifiers(
    a: feedback_amp |> intcode.with_input([phase_a]),
    b: feedback_amp |> intcode.with_input([phase_b]),
    c: feedback_amp |> intcode.with_input([phase_c]),
    d: feedback_amp |> intcode.with_input([phase_d]),
    e: feedback_amp |> intcode.with_input([phase_e]),
  )
  |> run_feedback_loop(with: 0)
}

fn run_feedback_loop(amps: Amplifiers, with input: Int) -> Result(Int, String) {
  use amp_a <- result.try(amps.a |> run_amp(with: input))
  use output_a <- result.try(amp_a |> get_amp_output)

  use amp_b <- result.try(amps.b |> run_amp(with: output_a))
  use output_b <- result.try(amp_b |> get_amp_output)

  use amp_c <- result.try(amps.c |> run_amp(with: output_b))
  use output_c <- result.try(amp_c |> get_amp_output)

  use amp_d <- result.try(amps.d |> run_amp(with: output_c))
  use output_d <- result.try(amp_d |> get_amp_output)

  use amp_e <- result.try(amps.e |> run_amp(with: output_d))
  use output_e <- result.try(amp_e |> get_amp_output)

  case amp_e |> intcode.state {
    intcode.Booted -> Error("Amp E is in Booted state")

    intcode.Blocked ->
      Amplifiers(
        amp_a |> intcode.purge_output,
        amp_b |> intcode.purge_output,
        amp_c |> intcode.purge_output,
        amp_d |> intcode.purge_output,
        amp_e |> intcode.purge_output,
      )
      |> run_feedback_loop(with: output_e)

    intcode.Halted -> Ok(output_e)
  }
}

fn run_amp(
  computer: intcode.Computer,
  with input: Int,
) -> Result(intcode.Computer, String) {
  computer
  |> intcode.with_input([input])
  |> intcode.run
  |> result.map_error(intcode.error_to_string)
}

fn get_amp_output(amp: intcode.Computer) -> Result(Int, String) {
  case amp |> intcode.output {
    [output] -> Ok(output)
    _ -> Error("Failed to get output for Amp")
  }
}
