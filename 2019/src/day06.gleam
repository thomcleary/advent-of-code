import gleam/dict
import gleam/list
import gleam/pair
import gleam/result
import gleam/string

pub const part1_answer = 223_251

pub fn part1(input: String) -> Result(Int, String) {
  use map <- result.try(parse_orbit_map(input))

  count_orbits(in: map)
}

pub fn part2(_input: String) -> Result(Int, String) {
  Ok(-1)
}

type Body {
  UniversalCenterOfMass(name: String)
  Body(name: String)
}

type Satellite {
  Satellite(name: String)
}

type Map =
  dict.Dict(Satellite, Body)

fn parse_orbit_map(input: String) -> Result(Map, String) {
  input
  |> string.split(on: "\n")
  |> list.try_fold(from: dict.new(), with: fn(map, line) {
    use #(body, satellite) <- result.map(
      line
      |> string.split_once(on: ")")
      |> result.replace_error("Invalid orbit [" <> line <> "]"),
    )

    dict.insert(into: map, for: Satellite(name: satellite), insert: case body {
      "COM" -> UniversalCenterOfMass(name: body)
      _ -> Body(name: body)
    })
  })
}

fn count_orbits(in map: Map) -> Result(Int, String) {
  let cache: dict.Dict(Satellite, Int) = dict.new()

  map
  |> dict.to_list
  |> list.try_fold(from: #(0, cache), with: fn(acc, orbit) {
    let #(satellite, body) = orbit
    let #(count, cache) = acc

    use body_orbits <- result.map(case dict.get(cache, Satellite(body.name)) {
      Ok(count) -> Ok(count)
      Error(_) -> count_indirect_orbits(of: body, with: map)
    })

    let satellite_orbits = body_orbits + 1

    #(
      count + satellite_orbits,
      cache
        |> dict.insert(for: Satellite(body.name), insert: body_orbits)
        |> dict.insert(for: satellite, insert: satellite_orbits),
    )
  })
  |> result.map(pair.first)
}

fn count_indirect_orbits(of body: Body, with map: Map) -> Result(Int, String) {
  count_indirect_orbits_go(of: body, from: 0, with: map)
}

fn count_indirect_orbits_go(
  of body: Body,
  from count: Int,
  with map: Map,
) -> Result(Int, String) {
  case body {
    UniversalCenterOfMass(_) -> Ok(count)

    Body(name:) -> {
      use next <- result.try(
        dict.get(map, Satellite(name))
        |> result.replace_error(
          "Failed to find body for satellite [" <> body.name <> "] in map",
        ),
      )

      count_indirect_orbits_go(of: next, from: count + 1, with: map)
    }
  }
}
