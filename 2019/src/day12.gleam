import gleam/dict
import gleam/int
import gleam/list
import gleam/order
import gleam/pair
import gleam/result
import gleam/string
import gleam_community/maths

pub const part1_answer = 6849

pub const part2_answer = 356_658_899_375_688

pub fn part1(input: String) -> Result(String, String) {
  use system <- result.try(parse_system(input))

  use simulation_result <- result.map(
    system |> simulate_motion(until: fn(_, steps) { Ok(steps == 1000) }),
  )

  simulation_result
  |> pair.first
  |> total_energy
  |> int.to_string
}

pub fn part2(input: String) -> Result(String, String) {
  use system_start <- result.try(parse_system(input))

  use x_cycle <- result.try(
    system_start
    |> simulate_motion_until(all: fn(steps, original_moon, sim_moon) {
      steps > 0
      && original_moon.position.x == sim_moon.position.x
      && original_moon.velocity.x == sim_moon.velocity.x
    }),
  )

  use y_cycle <- result.try(
    system_start
    |> simulate_motion_until(all: fn(steps, original_moon, sim_moon) {
      steps > 0
      && original_moon.position.y == sim_moon.position.y
      && original_moon.velocity.y == sim_moon.velocity.y
    }),
  )

  use z_cycle <- result.map(
    system_start
    |> simulate_motion_until(all: fn(steps, original_moon, sim_moon) {
      steps > 0
      && original_moon.position.z == sim_moon.position.z
      && original_moon.velocity.z == sim_moon.velocity.z
    }),
  )

  maths.lcm(x_cycle, maths.lcm(y_cycle, z_cycle))
  |> int.to_string
}

type Vec3 {
  Vec3(x: Int, y: Int, z: Int)
}

type Moon {
  Moon(position: Vec3, velocity: Vec3)
}

type System =
  dict.Dict(Int, Moon)

fn parse_system(input: String) -> Result(System, String) {
  input
  |> string.split(on: "\n")
  |> list.map(fn(line) {
    line
    |> string.split(on: ",")
    |> list.try_map(string.split_once(_, on: "="))
    |> result.replace_error("Failed to split axis on '='")
  })
  |> result.all
  |> result.try(fn(vectors) {
    vectors
    |> list.try_map(fn(vec) {
      case vec {
        [#(_, x), #(_, y), #(_, z)] -> {
          use x <- result.try(
            int.parse(x) |> result.replace_error("Failed to parse x axis"),
          )
          use y <- result.try(
            int.parse(y) |> result.replace_error("Failed to parse y axis"),
          )
          use z <- result.try(
            int.parse(z |> string.drop_end(1))
            |> result.replace_error("Failed to parse z axis"),
          )
          Ok(Moon(position: Vec3(x:, y:, z:), velocity: Vec3(x: 0, y: 0, z: 0)))
        }
        _ -> Error("Invalid moon position")
      }
    })
  })
  |> result.map(
    list.index_fold(_, from: dict.new(), with: fn(acc, moon, index) {
      acc
      |> dict.insert(index, moon)
    }),
  )
}

fn simulate_motion_until(
  system: System,
  all condition: fn(Int, Moon, Moon) -> Bool,
) -> Result(Int, String) {
  system
  |> simulate_motion(until: fn(system_sim, steps) {
    system
    |> dict.to_list
    |> list.try_fold(from: True, with: fn(is_match, entry) {
      let #(key, original) = entry

      use sim <- result.map(
        dict.get(system_sim, key)
        |> result.replace_error("Failed to get moon"),
      )

      is_match && condition(steps, original, sim)
    })
  })
  |> result.map(pair.second)
}

fn simulate_motion(
  system: System,
  until until: fn(System, Int) -> Result(Bool, String),
) -> Result(#(System, Int), String) {
  let pairs = system |> dict.keys |> list.combination_pairs

  do_simulate_motion(system:, pairs:, until:, steps: 0)
}

fn do_simulate_motion(
  system system: System,
  pairs pairs: List(#(Int, Int)),
  until until: fn(System, Int) -> Result(Bool, String),
  steps steps: Int,
) -> Result(#(System, Int), String) {
  use stop <- result.try(until(system, steps))

  case stop {
    True -> Ok(#(system, steps))
    False -> {
      use gravity_applied <- result.try(
        pairs
        |> list.try_fold(from: system, with: fn(acc, pair) {
          let #(left_key, right_key) = pair

          use left <- result.try(
            dict.get(acc, left_key)
            |> result.replace_error("Failed to get left moon"),
          )
          use right <- result.map(
            dict.get(acc, right_key)
            |> result.replace_error("Failed to get right moon"),
          )

          let #(next_left, next_right) = apply_gravity(left, right)

          acc
          |> dict.insert(left_key, next_left)
          |> dict.insert(right_key, next_right)
        }),
      )

      let velocity_applied =
        gravity_applied
        |> dict.map_values(fn(_, moon) { apply_velocity(moon) })

      do_simulate_motion(
        system: velocity_applied,
        pairs:,
        until:,
        steps: steps + 1,
      )
    }
  }
}

fn apply_gravity(left: Moon, right: Moon) -> #(Moon, Moon) {
  let #(x_left, x_right) =
    update_velocity(
      left_position: left.position.x,
      right_position: right.position.x,
      left_velocity: left.velocity.x,
      right_velocity: right.velocity.x,
    )
  let #(y_left, y_right) =
    update_velocity(
      left_position: left.position.y,
      right_position: right.position.y,
      left_velocity: left.velocity.y,
      right_velocity: right.velocity.y,
    )
  let #(z_left, z_right) =
    update_velocity(
      left_position: left.position.z,
      right_position: right.position.z,
      left_velocity: left.velocity.z,
      right_velocity: right.velocity.z,
    )

  #(
    Moon(..left, velocity: Vec3(x: x_left, y: y_left, z: z_left)),
    Moon(..right, velocity: Vec3(x: x_right, y: y_right, z: z_right)),
  )
}

fn update_velocity(
  left_position left_pos: Int,
  right_position right_pos: Int,
  left_velocity left_velocity: Int,
  right_velocity right_velocity: Int,
) -> #(Int, Int) {
  case int.compare(left_pos, right_pos) {
    order.Eq -> #(left_velocity, right_velocity)
    order.Lt -> #(left_velocity + 1, right_velocity - 1)
    order.Gt -> #(left_velocity - 1, right_velocity + 1)
  }
}

fn apply_velocity(moon: Moon) -> Moon {
  Moon(
    ..moon,
    position: Vec3(
      x: moon.position.x + moon.velocity.x,
      y: moon.position.y + moon.velocity.y,
      z: moon.position.z + moon.velocity.z,
    ),
  )
}

fn total_energy(system: System) -> Int {
  system
  |> dict.values
  |> list.map(fn(moon) {
    let potential_energy = sum_coordinates(moon.position)
    let kinetic_energy = sum_coordinates(moon.velocity)

    potential_energy * kinetic_energy
  })
  |> int.sum
}

fn sum_coordinates(vec: Vec3) -> Int {
  int.absolute_value(vec.x)
  + int.absolute_value(vec.y)
  + int.absolute_value(vec.z)
}
