import day_01

pub fn day_01_test() {
  assert day_01.part1([12]) == 2
  assert day_01.part1([14]) == 2
  assert day_01.part1([1969]) == 654
  assert day_01.part1([100_756]) == 33_583
}
