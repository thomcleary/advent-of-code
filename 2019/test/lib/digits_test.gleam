import lib/digits

pub fn from_int_test() {
  assert digits.from_int(0) == [0]
  assert digits.from_int(1) == [1]
  assert digits.from_int(-1) == [1]
  assert digits.from_int(10) == [1, 0]
  assert digits.from_int(123) == [1, 2, 3]
  assert digits.from_int(-123) == [1, 2, 3]
  assert digits.from_int(9_876_543_210) == [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
}
