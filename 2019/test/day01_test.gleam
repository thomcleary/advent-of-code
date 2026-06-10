import day01

pub fn part1_example1_test() {
  assert day01.part1([12]) == 2
}

pub fn part1_example2_test() {
  assert day01.part1([14]) == 2
}

pub fn part1_example3_test() {
  assert day01.part1([1969]) == 654
}

pub fn part1_example4_test() {
  assert day01.part1([100_756]) == 33_583
}

pub fn part2_example1_test() {
  assert day01.part2([14]) == 2
}

pub fn part2_example2_test() {
  assert day01.part2([1969]) == 966
}

pub fn part2_example3_test() {
  assert day01.part2([100_756]) == 50_346
}
