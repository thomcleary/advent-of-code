/*
Day 3: Mull It Over
https://adventofcode.com/2024/day/3
*/

#define _DEFAULT_SOURCE
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>

// #define USE_EXAMPLE
#include "../lib/aoc.h"
#include "main.h"

bool find_m(void) {
  int ch;
  while ((ch = getchar()) != 'm' && ch != EOF) {
    ;
  }

  if (ch == 'm') {
    ungetc(ch, stdin);
    return true;
  }

  return false;
}

int main(void) {
  long total = 0;

  while (find_m()) {
    long x, y;
    char closing;
    int matched = scanf("mul(%ld,%ld%c", &x, &y, &closing);

    if (matched == 3 && closing == ')') {
      total += x * y;
    }
  }

  print_day(0, "Template");
  printf("Part 1: %ld\n", total);

  assert(total == PART1_ANSWER);
  // assert(0 == PART2_ANSWER);

  return 0;
}
