import day03.{Down, Left, Right, Segment, Up, Wire}

pub fn part1_example1_test() {
  assert day03.part1(example_1) == Ok(6)
}

pub fn part1_example2_test() {
  assert day03.part1(example_2) == Ok(159)
}

pub fn part1_example3_test() {
  assert day03.part1(example_3) == Ok(135)
}

pub fn part2_example1_test() {
  assert day03.part2(example_1) == Ok(30)
}

pub fn part2_example2_test() {
  assert day03.part2(example_2) == Ok(610)
}

pub fn part2_example3_test() {
  assert day03.part2(example_3) == Ok(410)
}

const example_1 = #(
  Wire(
    [
      Segment(Right, 8),
      Segment(Up, 5),
      Segment(Left, 5),
      Segment(Down, 3),
    ],
  ),
  Wire(
    [
      Segment(Up, 7),
      Segment(Right, 6),
      Segment(Down, 4),
      Segment(Left, 4),
    ],
  ),
)

const example_2 = #(
  Wire(
    [
      Segment(Right, 75),
      Segment(Down, 30),
      Segment(Right, 83),
      Segment(Up, 83),
      Segment(Left, 12),
      Segment(Down, 49),
      Segment(Right, 71),
      Segment(Up, 7),
      Segment(Left, 72),
    ],
  ),
  Wire(
    [
      Segment(Up, 62),
      Segment(Right, 66),
      Segment(Up, 55),
      Segment(Right, 34),
      Segment(Down, 71),
      Segment(Right, 55),
      Segment(Down, 58),
      Segment(Right, 83),
    ],
  ),
)

const example_3 = #(
  Wire(
    [
      Segment(Right, 98),
      Segment(Up, 47),
      Segment(Right, 26),
      Segment(Down, 63),
      Segment(Right, 33),
      Segment(Up, 87),
      Segment(Left, 62),
      Segment(Down, 20),
      Segment(Right, 33),
      Segment(Up, 53),
      Segment(Right, 51),
    ],
  ),
  Wire(
    [
      Segment(Up, 98),
      Segment(Right, 91),
      Segment(Down, 20),
      Segment(Right, 16),
      Segment(Down, 67),
      Segment(Right, 40),
      Segment(Up, 7),
      Segment(Right, 15),
      Segment(Up, 6),
      Segment(Right, 7),
    ],
  ),
)
