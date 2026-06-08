import day_01

pub fn day_01_part1_test() {
  assert day_01.part_1([12]) == 2
  assert day_01.part_1([14]) == 2
  assert day_01.part_1([1969]) == 654
  assert day_01.part_1([100_756]) == 33_583
}

pub fn day_01_part2_test() {
  assert day_01.part_2([14]) == 2
  assert day_01.part_2([1969]) == 966
  assert day_01.part_2([100_756]) == 50_346
}
