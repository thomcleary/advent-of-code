/*
Day 0: Template
https://adventofcode.com/2024/day/0
*/

#define _DEFAULT_SOURCE
#include <assert.h>
#include <stdio.h>

#include "../lib/aoc.h"
#include "../lib/txt.h"

#define USE_EXAMPLE

#ifdef USE_EXAMPLE
#define PART1_ANSWER 1
#define PART2_ANSWER 2
#else
#define PART1_ANSWER 1
#define PART2_ANSWER 2
#endif

int main(void) {
  Txt *txt = txt_read(stdin);

  txt_print(txt);

  txt_free(txt);

  print_day(0, "Template");
  print_part(1, 0, PART1_ANSWER);
  print_part(2, 0, PART2_ANSWER);

  return 0;
}
