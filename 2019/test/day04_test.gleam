import day04

pub fn part1_example1_test() {
  let password = 111_111
  let password_range = day04.PasswordRange(min: password, max: password)

  assert day04.part1(password_range) == Ok(1)
}

pub fn part1_example2_test() {
  let password = 223_450
  let password_range = day04.PasswordRange(min: password, max: password)

  assert day04.part1(password_range) == Ok(0)
}

pub fn part1_example3_test() {
  let password = 123_789
  let password_range = day04.PasswordRange(min: password, max: password)

  assert day04.part1(password_range) == Ok(0)
}
