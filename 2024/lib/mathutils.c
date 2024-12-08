#include "./mathutils.h"
#include <limits.h>
#include <stdint.h>

bool add_int64_ok(int64_t a, int64_t b) {
  return a <= INT64_MAX - b;
}

bool mult_int64_ok(int64_t a, int64_t b) {
  if (a == 0 || b == 0) {
    return true;
  }

  if (a > 0) {
    if (b > 0) {
      return a <= (INT64_MAX / b);
    }
    // b < 0
    return b >= (INT64_MIN / a);
  }

  // a < 0
  if (b > 0) {
    return a >= (INT64_MIN / b);
  }
  // b < 0
  return a >= (INT64_MAX / b);
}
