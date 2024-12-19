#include "types.h"

Option some(void *value) {
  return (Option){.some = true, .value = value};
}

Option none(void) {
  return (Option){.some = false, .value = NULL};
}
