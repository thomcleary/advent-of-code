import day_01

pub fn part1_test() {
  assert day_01.part1([12]) == 2
  assert day_01.part1([14]) == 2
  assert day_01.part1([1969]) == 654
  assert day_01.part1([100_756]) == 33_583
}

pub fn part2_test() {
  assert day_01.part2([14]) == 2
  assert day_01.part2([1969]) == 966
  assert day_01.part2([100_756]) == 50_346
}
