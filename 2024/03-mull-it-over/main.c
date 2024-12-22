/*
Day 3: Mull It Over
https://adventofcode.com/2024/day/3
*/

#define _DEFAULT_SOURCE
#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../lib/aoc.h"
#include "../lib/math-utils.h"

// #define USE_EXAMPLE

#ifdef USE_EXAMPLE
#define PART1_ANSWER 161
#define PART2_ANSWER 48
#else
#define PART1_ANSWER 189600467
#define PART2_ANSWER 107069718
#endif

typedef struct MemoryState {
  char *memory;
  char *pos;
  bool do_mul;
  uint64_t result;
} MemoryState;

void free_memory_state(MemoryState *state) {
  free(state->memory);
  free(state);
}

MemoryState *read_memory(void) {
  size_t size = BUFSIZ;
  size_t length = 0;
  char *memory = malloc(size);
  assert(memory != NULL && "malloc failed");

  size_t bytes_read;
  while ((bytes_read = fread(memory + length, 1, BUFSIZ, stdin)) == BUFSIZ) {
    size += BUFSIZ;
    length += BUFSIZ;
    memory = realloc(memory, size);
    assert(memory != NULL && "realloc failed");
  }
  assert(ferror(stdin) == 0 && "fread failed");

  length += bytes_read;
  memory[length] = '\0';

  MemoryState *state = malloc(sizeof(*state));
  assert(state != NULL && "malloc failed");
  state->memory = memory;
  state->pos = state->memory;
  state->do_mul = true;
  state->result = 0;

  return state;
}

bool next_mul(MemoryState *state) {
  assert(state != NULL && "state is NULL");

  while (*state->pos && *state->pos != 'm') {
    if (*state->pos == 'd') {
      const char *dont = "don't()";
      size_t dont_len = strlen(dont);
      const char *doo = "do()";
      size_t doo_len = strlen(doo);

      if (strncmp(state->pos, dont, dont_len) == 0) {
        state->do_mul = false;
        state->pos = state->pos + dont_len - 1;
      } else if (strncmp(state->pos, doo, strlen(doo)) == 0) {
        state->do_mul = true;
        state->pos = state->pos + doo_len - 1;
      }
    }

    state->pos++;
  }

  if (*state->pos != 'm') {
    return false;
  }

  int64_t x, y;
  char closing;
  int matched =
      sscanf(state->pos, "mul(%" PRId64 ",%" PRId64 "%c", &x, &y, &closing);

  state->pos++;

  if (!(matched == 3 && closing == ')')) {
    return next_mul(state);
  }

  assert(mult_int64_ok(x, y));
  state->result = (uint64_t)(x * y);
  return true;
}

int main(void) {
  MemoryState *state = read_memory();
  uint64_t unconditional_total = 0;
  uint64_t conditional_total = 0;

  while (next_mul(state)) {
    unconditional_total += state->result;

    if (state->do_mul) {
      conditional_total += state->result;
    }
  }

  free_memory_state(state);

  print_day(3, "Mull It Over");
  print_part_uint64(1, unconditional_total, PART1_ANSWER);
  print_part_uint64(2, conditional_total, PART2_ANSWER);

  return 0;
}
