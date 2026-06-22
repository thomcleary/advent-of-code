import gleam/int
import gleam/list
import gleam/option
import gleam/result
import lib/intcode

pub const part1_answer = 92_663

pub fn part1(input: String) -> Result(Int, String) {
  use amplifier_controller_software <- result.try(
    intcode.parse_program(input)
    |> result.map_error(intcode.error_to_string),
  )

  find_highest_thruster_signal(with: amplifier_controller_software)
}

pub fn part2(input: String) -> Result(Int, String) {
  Ok(-1)
}

type PhaseSettings =
  #(Int, Int, Int, Int, Int)

fn find_highest_thruster_signal(
  with program: intcode.Program,
) -> Result(Int, String) {
  use phase_setting_permutations <- result.try(
    int.range(from: 4, to: -1, with: [], run: list.prepend)
    |> list.permutations
    |> list.try_map(range_to_phase_settings),
  )

  use highest <- result.try({
    use acc, phase_settings <- list.try_fold(
      over: phase_setting_permutations,
      from: option.None,
    )

    use thruster_signal <- result.try(
      program |> run_amplifiers(with: phase_settings),
    )

    acc
    |> option.map(with: int.max(_, thruster_signal))
    |> option.unwrap(or: thruster_signal)
    |> option.Some
    |> Ok
  })

  case highest {
    option.Some(n) -> Ok(n)
    option.None -> Error("Failed to find any thruster signal")
  }
}

fn range_to_phase_settings(list: List(Int)) -> Result(PhaseSettings, String) {
  case list {
    [a, b, c, d, e] -> Ok(#(a, b, c, d, e))
    _ -> Error("Invalid phase settings")
  }
}

fn run_amplifiers(
  program: intcode.Program,
  with phase_settings: PhaseSettings,
) -> Result(Int, String) {
  let #(phase_a, phase_b, phase_c, phase_d, phase_e) = phase_settings

  let amp_a = program |> intcode.boot
  let amp_b = program |> intcode.boot
  let amp_c = program |> intcode.boot
  let amp_d = program |> intcode.boot
  let amp_e = program |> intcode.boot

  use output_a <- result.try(
    amp_a |> run_amplifier(phase_setting: phase_a, input: 0),
  )
  use output_b <- result.try(
    amp_b |> run_amplifier(phase_setting: phase_b, input: output_a),
  )
  use output_c <- result.try(
    amp_c |> run_amplifier(phase_setting: phase_c, input: output_b),
  )
  use output_d <- result.try(
    amp_d |> run_amplifier(phase_setting: phase_d, input: output_c),
  )
  amp_e |> run_amplifier(phase_setting: phase_e, input: output_d)
}

fn run_amplifier(
  computer: intcode.Computer,
  phase_setting phase: Int,
  input input: Int,
) -> Result(Int, String) {
  computer
  |> intcode.with_input([phase, input])
  |> intcode.run
  |> result.map_error(intcode.error_to_string)
  |> result.try(fn(computer) {
    computer
    |> intcode.output
    |> list.first
    |> result.replace_error("Failed to get output for Amp")
  })
}
