/*
Day 1: Hystorian Hysteria
https://adventofcode.com/2024/day/1
*/

#define _POSIX_C_SOURCE 200809L
#include <assert.h>
#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

// #define USE_EXAMPLE
#include "../lib/aoc.h"
#include "main.h"

int compare_int(const void *a, const void *b) {
  return *(int *)a - *(int *)b;
}

int main(void) {
  print_day(1, "Hystorian Hysteria");

  // There's only 1000 lines in the puzzle input, 1024 will do
  long int left_ids[BUFSIZ], right_ids[BUFSIZ];
  int num_lines = 0;

  char *line = NULL;
  size_t line_len = 0;
  errno = 0;

  while ((getline(&line, &line_len, stdin) != -1)) {
    long int left, right;
    if (sscanf(line, "%ld %ld", &left, &right) != 2) {
      free(line);
      perror("sscanf failed");
      exit(EXIT_FAILURE);
    };

    left_ids[num_lines] = left;
    right_ids[num_lines] = right;
    num_lines++;
  }

  free(line);

  if (errno != 0) {
    perror("getline failed");
    exit(EXIT_FAILURE);
  }

  qsort(left_ids, num_lines, sizeof(*left_ids), compare_int);
  qsort(right_ids, num_lines, sizeof(*right_ids), compare_int);

  long int total_distance = 0;
  for (int i = 0; i < num_lines; i++) {
    total_distance += labs(left_ids[i] - right_ids[i]);
  }

  printf("Part 1: %ld\n", total_distance);
  assert(total_distance == PART1_ANSWER);

  return 0;
}
