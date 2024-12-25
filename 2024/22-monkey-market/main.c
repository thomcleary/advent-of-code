/*
Day 22: Monkey Market
https://adventofcode.com/2024/day/22
*/

#define _DEFAULT_SOURCE
#include <assert.h>
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "../lib/aoc.h"
#include "../lib/txt.h"

// #define USE_EXAMPLE

#ifdef USE_EXAMPLE
#define INPUT_FILENAME "example-input.txt"
#define PART1_ANSWER 37327623
#define PART2_ANSWER 2
#else
#define INPUT_FILENAME "puzzle-input.txt"
#define PART1_ANSWER 20071921341
#define PART2_ANSWER 2
#endif

uint64_t prune(uint64_t secret) {
  return secret % 16777216;
}

uint64_t mix(uint64_t secret, uint64_t mixer) {
  return secret ^ mixer;
}

uint64_t step_one(uint64_t secret) {
  return prune(mix(secret, secret * 64));
}

uint64_t step_two(uint64_t secret) {
  return prune(mix(secret, secret / 32));
}

uint64_t step_three(uint64_t secret) {
  return prune(mix(secret, secret * 2048));
}

uint64_t nth_secret(uint64_t secret, uint64_t n) {
  for (; n > 0; n--) {
    secret = step_three(step_two(step_one(secret)));
  }

  return secret;
}

uint64_t secret_total(Txt *txt) {
  uint64_t total = 0;

  for (size_t i = 0; i < txt->num_lines; i++) {
    errno = 0;
    uint64_t initial_secret = strtoull(txt->lines[i], NULL, 10);
    assert(errno == 0 && "strtoull failed");

    total += nth_secret(initial_secret, 2000);
  }

  return total;
}

int main(void) {
  Txt *txt = txt_read_file(INPUT_FILENAME);

  uint64_t total = secret_total(txt);

  txt_free(txt);

  print_day(22, "Monkey Market");
  print_part_uint64(1, total, PART1_ANSWER);
  // print_part_uint64(2, 0, PART2_ANSWER);

  return 0;
}
