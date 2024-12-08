/*
Day 7: Bridge Repair
https://adventofcode.com/2024/day/7
*/

// #define USE_EXAMPLE
#define _DEFAULT_SOURCE

#include <assert.h>
#include <errno.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../lib/aoc.h"
#include "../lib/mathutils.h"
#include "../lib/txt.h"
#include "main.h"

typedef struct Equation {
  int64_t test_value;
  int64_t *operands;
  size_t num_operands;
} Equation;

typedef struct Calibration {
  Equation *equations;
  size_t num_equations;
} Calibration;

void calibration_free(Calibration *calibration) {
  for (size_t i = 0; i < calibration->num_equations; i++) {
    free(calibration->equations[i].operands);
  }
  free(calibration->equations);
  free(calibration);
}

void calibration_print(Calibration *calibration) {
  for (size_t i = 0; i < calibration->num_equations; i++) {
    Equation eq = calibration->equations[i];
    printf("%" PRId64 ": ", eq.test_value);
    for (size_t j = 0; j < eq.num_operands; j++) {
      printf("%" PRId64 " ", eq.operands[j]);
    }
    printf("\n");
  }
  printf("\n");
}

Calibration *calibration_parse(Txt *txt) {
  Equation *equations = malloc(sizeof(*equations) * txt->num_lines);
  assert(equations != NULL && "malloc failed");

  for (size_t i = 0; i < txt->num_lines; i++) {
    char *line = txt->lines[i];
    char *token = strsep(&line, ":");
    line++; // Skip space between ':' and first operand

    errno = 0;
    int64_t test_value = strtoll(token, NULL, 10);
    assert(errno == 0 && "strtoll failed");

    size_t operands_size = 16; // puzzle input has 12 operands max
    int64_t *operands = malloc(sizeof(*operands) * operands_size);
    size_t num_operands = 0;
    while ((token = strsep(&line, " ")) != NULL) {
      errno = 0;
      int64_t operand = strtoll(token, NULL, 10);
      assert(errno == 0 && "strtoll failed");

      if (num_operands == operands_size) {
        operands_size *= 2;
        operands = realloc(operands, sizeof(*operands) * operands_size);
        assert(operands != NULL && "realloc failed");
      }

      operands[num_operands++] = operand;
    }

    equations[i].test_value = test_value;
    equations[i].operands = operands;
    equations[i].num_operands = num_operands;
  }

  Calibration *calibration = malloc(sizeof(*calibration));
  assert(calibration != NULL && "malloc failed");
  calibration->equations = equations;
  calibration->num_equations = txt->num_lines;

  return calibration;
}

bool equation_search(Equation eq, int64_t value, int64_t *rest, size_t num_rest,
                     bool concat) {
  if (num_rest == 0) {
    return eq.test_value == value;
  }

  if (eq.test_value < value) {
    return false;
  }

  int64_t operand = *rest;
  rest++;
  num_rest--;

  // mult and concat will also overflow if add overflows
  if (!add_int64_ok(value, operand)) {
    return false;
  }

  if (concat) {
    const size_t int64_decimal_max_len = 19;
    const size_t concat_buf_len = (int64_decimal_max_len * 2) + 1;
    char concat_buf[concat_buf_len];
    snprintf(concat_buf, concat_buf_len, "%" PRId64 "%" PRId64, value, operand);

    errno = 0;
    int64_t concat_value = strtoll(concat_buf, NULL, 10);
    assert(errno != EINVAL && "strtoll failed");

    if (errno != ERANGE &&
        equation_search(eq, concat_value, rest, num_rest, concat)) {
      return true;
    }
  }

  // Try mult before add, it'll probably find answer or fail faster
  bool mult_result =
      (mult_int64_ok(value, operand) &&
       equation_search(eq, value * operand, rest, num_rest, concat));

  return mult_result ||
         equation_search(eq, value + operand, rest, num_rest, concat);
}

int64_t calibration_search(Calibration *calibration, bool concat) {
  int64_t calibration_result = 0;

  for (size_t i = 0; i < calibration->num_equations; i++) {
    Equation eq = calibration->equations[i];

    if (equation_search(eq, 0, eq.operands, eq.num_operands, concat)) {
      calibration_result += eq.test_value;
    }
  }

  return calibration_result;
}

int main(void) {
  Txt *txt = txt_read(stdin);
  Calibration *calibration = calibration_parse(txt);

  int64_t add_mult_result = calibration_search(calibration, false);
  int64_t add_mult_concat_result = calibration_search(calibration, true);

  calibration_free(calibration);
  txt_free(txt);

  print_day(7, "Bridge Repair");
  printf("Part 1: %" PRId64 "\n", add_mult_result);
  printf("Part 2: %" PRId64 "\n", add_mult_concat_result);

  assert(add_mult_result == PART1_ANSWER);
  assert(add_mult_concat_result == PART2_ANSWER);

  return 0;
}
