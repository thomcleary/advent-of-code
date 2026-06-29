import day10
import gleam/int
import gleam/result
import lib/aoc

const large_example = ".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##"

pub fn part1_example1_test() {
  assert day10.part1(
      ".#..#
.....
#####
....#
...##",
    )
    == Ok(int.to_string(8))
}

pub fn part1_example2_test() {
  assert day10.part1(
      "......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####",
    )
    == Ok(int.to_string(33))
}

pub fn part1_example3_test() {
  assert day10.part1(
      "#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.",
    )
    == Ok(int.to_string(35))
}

pub fn part1_example4_test() {
  assert day10.part1(
      ".#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..",
    )
    == Ok(int.to_string(41))
}

pub fn part1_example5_test() {
  assert day10.part1(large_example) == Ok(int.to_string(210))
}

pub fn part1_test() {
  assert aoc.Day10
    |> aoc.read_input
    |> result.try(day10.part1)
    == Ok(int.to_string(day10.part1_answer))
}

pub fn part2_example_test() {
  assert day10.part2(large_example) == Ok(int.to_string(802))
}

pub fn part2_test() {
  assert aoc.Day10
    |> aoc.read_input
    |> result.try(day10.part2)
    == Ok(int.to_string(day10.part2_answer))
}
