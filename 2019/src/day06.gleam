import gleam/dict
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam/set
import gleam/string

pub const part1_answer = 223_251

pub const part2_answer = 430

pub fn part1(input: String) -> Result(String, String) {
  use orbits <- result.try(parse_orbits(input))

  orbits
  |> count_direct_and_indirect
  |> result.map(int.to_string)
}

pub fn part2(input: String) -> Result(String, String) {
  use orbits <- result.try(parse_orbits(input))

  orbits
  |> find_minimum_transfers(from: Satellite("YOU"), to: Satellite("SAN"))
  |> result.map(int.to_string)
}

type Body {
  Body(name: String)
}

type Satellite {
  Satellite(name: String)
}

type Orbits {
  Orbits(map: Map, neighbours: Neighbours)
}

type Map =
  dict.Dict(Satellite, Body)

type Neighbours =
  dict.Dict(String, set.Set(String))

fn parse_orbits(input: String) -> Result(Orbits, String) {
  input
  |> string.split(on: "\n")
  |> list.try_fold(
    from: Orbits(map: dict.new(), neighbours: dict.new()),
    with: fn(orbits, line) {
      use #(body_name, satellite_name) <- result.map(
        line
        |> string.split_once(on: ")")
        |> result.replace_error("Invalid orbit [" <> line <> "]"),
      )

      let body = Body(name: body_name)
      let satellite = Satellite(name: satellite_name)

      Orbits(
        map: orbits.map
          |> dict.insert(for: satellite, insert: body),
        neighbours: orbits.neighbours
          |> dict.upsert(update: body.name, with: fn(prev) {
            prev
            |> option.unwrap(or: set.new())
            |> set.insert(satellite.name)
          })
          |> dict.upsert(update: satellite.name, with: fn(prev) {
            prev
            |> option.unwrap(or: set.new())
            |> set.insert(body.name)
          }),
      )
    },
  )
}

fn count_direct_and_indirect(orbits: Orbits) -> Result(Int, String) {
  orbits.map
  |> dict.values
  |> list.try_fold(from: 0, with: fn(count, body) {
    use body_orbits <- result.map(count_indirect_orbits(
      of: body,
      from: 0,
      with: orbits.map,
    ))

    count + 1 + body_orbits
  })
}

fn count_indirect_orbits(
  of body: Body,
  from count: Int,
  with map: Map,
) -> Result(Int, String) {
  case body {
    Body("COM") -> Ok(count)
    Body(name:) -> {
      use next <- result.try(
        dict.get(map, Satellite(name))
        |> result.replace_error(
          "Failed to find body for satellite [" <> body.name <> "] in the map",
        ),
      )

      count_indirect_orbits(of: next, from: count + 1, with: map)
    }
  }
}

fn find_minimum_transfers(
  orbits: Orbits,
  from start: Satellite,
  to target: Satellite,
) -> Result(Int, String) {
  use from <- result.try(
    dict.get(orbits.map, start)
    |> result.replace_error(
      "Failed to find body that [" <> start.name <> "] is orbiting in the map",
    ),
  )

  use to <- result.try(
    dict.get(orbits.map, target)
    |> result.replace_error(
      "Failed to find body that [" <> target.name <> "] is orbiting in the map",
    ),
  )

  do_find_minimum_transfers(
    from:,
    to:,
    previous: start.name,
    neighbours: orbits.neighbours,
    transfers: 0,
  )
}

fn do_find_minimum_transfers(
  from from: Body,
  to to: Body,
  previous previous: String,
  neighbours neighbours: Neighbours,
  transfers transfers: Int,
) -> Result(Int, String) {
  case from {
    Body(name) if name == to.name -> Ok(transfers)
    _ ->
      neighbours
      |> dict.get(from.name)
      |> result.unwrap(or: set.new())
      |> set.delete(previous)
      |> set.to_list
      |> list.find_map(with: fn(neighbour) {
        do_find_minimum_transfers(
          from: Body(name: neighbour),
          to:,
          previous: from.name,
          neighbours:,
          transfers: transfers + 1,
        )
      })
      |> result.replace_error(
        "Failed to find minimum transfers to ["
        <> to.name
        <> "] from ["
        <> from.name
        <> "]",
      )
  }
}
