#ifndef TYPES_H
#define TYPES_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

typedef int64_t (*CompareFn)(const void *, const void *);

typedef struct {
  bool some;
  void *value;
} Option;

Option some(void *value);
Option none(void);

#endif
