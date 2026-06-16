import gleam/int

pub fn from_int(n: Int) -> List(Int) {
  case n {
    0 -> [0]
    _ ->
      n
      |> int.absolute_value
      |> from_int_go([])
  }
}

fn from_int_go(n: Int, digits: List(Int)) -> List(Int) {
  case n > 0 {
    True -> from_int_go(n / 10, [n % 10, ..digits])
    False -> digits
  }
}
