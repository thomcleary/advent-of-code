/*
Day 3: Mull It Over
https://adventofcode.com/2024/day/3
*/

// #define USE_EXAMPLE
#define _DEFAULT_SOURCE

#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../lib/aoc.h"
#include "../lib/mathutils.h"
#include "main.h"

typedef struct MemoryState {
  char *memory;
  char *pos;
  bool do_mul;
  int64_t result;
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
  state->result = x * y;
  return true;
}

int main(void) {
  MemoryState *state = read_memory();
  int64_t unconditional_total = 0;
  int64_t conditional_total = 0;

  while (next_mul(state)) {
    unconditional_total += state->result;

    if (state->do_mul) {
      conditional_total += state->result;
    }
  }

  free_memory_state(state);

  print_day(3, "Mull It Over");
  printf("Part 1: %" PRId64 "\n", unconditional_total);
  printf("Part 2: %" PRId64 "\n", conditional_total);

  assert(unconditional_total == PART1_ANSWER);
  assert(conditional_total == PART2_ANSWER);

  return 0;
}
