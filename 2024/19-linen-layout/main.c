/*
Day 19: Linen Layout
https://adventofcode.com/2024/day/19
*/

#define _DEFAULT_SOURCE
#include <assert.h>
#include <inttypes.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "../lib/aoc.h"
#include "../lib/hashtable.h"
#include "../lib/txt.h"

// #define USE_EXAMPLE

#ifdef USE_EXAMPLE
#define INPUT_FILENAME "example-input.txt"
#define PART1_ANSWER 6
#define PART2_ANSWER 2
#else
#define INPUT_FILENAME "puzzle-input.txt"
#define PART1_ANSWER 365
#define PART2_ANSWER 2
#endif

typedef struct {
  Hashtable *patterns;
  char **designs;
  size_t num_designs;
} Towels;

void towels_free(Towels *towels) {
  hashtable_free(towels->patterns);
  for (size_t i = 0; i < towels->num_designs; i++) {
    free(towels->designs[i]);
  }
  free(towels->designs);
}

Towels towels_parse(Txt *txt) {
  Towels towels;

  SplitTxt *split_txt = txt_split(txt, "");
  assert(split_txt->num_txts == 2 && "invalid puzzle input");

  Txt patterns_txt = split_txt->txts[0];
  assert(patterns_txt.num_lines == 1 && "invalid puzzle input");

  towels.patterns = hashtable_new();
  char *patterns_line = patterns_txt.lines[0];
  char *pattern;

  while ((pattern = strsep(&patterns_line, " "))) {
    size_t pattern_len = strlen(pattern);
    if (pattern[pattern_len - 1] == ',') {
      pattern[pattern_len - 1] = '\0';
    }

    hashtable_set(towels.patterns, pattern, NULL);
  }

  Txt designs_txt = split_txt->txts[1];
  towels.num_designs = designs_txt.num_lines;
  towels.designs = malloc(sizeof(*towels.designs) * towels.num_designs);
  assert(towels.designs != NULL && "malloc failed");

  for (size_t i = 0; i < towels.num_designs; i++) {
    towels.designs[i] = strdup(designs_txt.lines[i]);
    assert(towels.designs[i] != NULL && "strdup failed");
  }

  return towels;
}

bool is_design_valid(char *design, Hashtable *patterns, Hashtable *cache) {
  Option cache_option = hashtable_get(cache, design);
  if (cache_option.some) {
    return (bool)cache_option.value;
  }

  int64_t design_len = (int64_t)strlen(design);

  char window[design_len + 1];
  int64_t window_head = 0;
  int64_t window_tail = design_len - 1;

  bool found = false;

  while (window_head <= window_tail) {
    int64_t window_len = window_tail - window_head + 1;
    memset(window, 0, sizeof(window));
    strncpy(window, design + window_head, window_len);

    if (hashtable_has(patterns, window)) {
      char *next = &design[window_tail + 1];
      if (!(*next) || is_design_valid(next, patterns, cache)) {
        found = true;
        break;
      }
    }

    window_tail--;
  }

  hashtable_set(cache, design, (void *)found);

  return found;
}

uint64_t valid_towel_designs(Towels *towels) {
  Hashtable *cache = hashtable_new();
  uint64_t valid = 0;

  for (size_t i = 0; i < towels->num_designs; i++) {
    if (is_design_valid(towels->designs[i], towels->patterns, cache)) {
      valid++;
    }
  }

  hashtable_free(cache);

  return valid;
}

int main(void) {
  Txt *txt = txt_read_file(INPUT_FILENAME);

  Towels towels = towels_parse(txt);
  uint64_t num_valid_designs = valid_towel_designs(&towels);

  towels_free(&towels);
  txt_free(txt);

  print_day(19, "Linen Layout");
  print_part_uint64(1, num_valid_designs, PART1_ANSWER);
  // print_part_uint64(2, 0, PART2_ANSWER);

  return 0;
}
