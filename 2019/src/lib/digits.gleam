import gleam/int

pub fn from_int(n: Int) -> List(Int) {
  case n {
    0 -> [0]
    _ ->
      n
      |> int.absolute_value
      |> do_from_int([])
  }
}

fn do_from_int(n: Int, digits: List(Int)) -> List(Int) {
  case n > 0 {
    True -> do_from_int(n / 10, [n % 10, ..digits])
    False -> digits
  }
}
