/*
Day 0: Template
https://adventofcode.com/2024/day/0
*/

#define USE_EXAMPLE
#define _DEFAULT_SOURCE

#include <assert.h>
#include <stdio.h>

#include "../lib/aoc.h"
#include "../lib/txt.h"
#include "main.h"

int main(void) {
  Txt *txt = txt_read(stdin);

  txt_print(txt);

  txt_free(txt);

  print_day(0, "Template");
  print_part(1, 0, PART1_ANSWER);
  print_part(2, 0, PART2_ANSWER);

  return 0;
}
