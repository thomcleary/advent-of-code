/*
Day 1: Hystorian Hysteria
https://adventofcode.com/2024/day/1
*/

#define _DEFAULT_SOURCE
#include <assert.h>
#include <errno.h>
#include <inttypes.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "../lib/aoc.h"
#include "../lib/hashtable.h"
#include "../lib/types.h"

// #define USE_EXAMPLE

#ifdef USE_EXAMPLE
#define PART1_ANSWER 11
#define PART2_ANSWER 31
#else
#define PART1_ANSWER 3246517
#define PART2_ANSWER 29379307
#endif

size_t get_ids(int64_t left_ids[], int64_t right_ids[]) {
  size_t num_lines = 0;
  char *line = NULL;
  size_t line_len = 0;
  errno = 0;

  while ((getline(&line, &line_len, stdin) != -1)) {
    int64_t left, right;
    int matched = sscanf(line, "%" PRId64 "%" PRId64, &left, &right);
    assert(matched == 2 && "sscanf failed");

    left_ids[num_lines] = left;
    right_ids[num_lines] = right;
    num_lines++;
  }

  free(line);
  assert(errno == 0 && "getline failed");

  return num_lines;
}

int compare_int64(const void *a, const void *b) {
  int64_t a_value = *(int64_t *)a;
  int64_t b_value = *(int64_t *)b;

  if (a_value == b_value) {
    return 0;
  }

  if (a_value < b_value) {
    return -1;
  }

  return 1;
}

char *get_key(int64_t id) {
  char *key = malloc(sizeof(*key) * 8); // All IDs are only 5 chars long
  assert(key != NULL && "malloc failed");

  int written = sprintf(key, "%" PRId64, id);
  assert(written != -1 && "sprintf failed");

  return key;
}

int main(void) {
  // There's only 1000 lines in the puzzle input, 1024 will do
  int64_t left_ids[BUFSIZ], right_ids[BUFSIZ];
  size_t num_ids = get_ids(left_ids, right_ids);

  qsort(left_ids, num_ids, sizeof(*left_ids), compare_int64);
  qsort(right_ids, num_ids, sizeof(*right_ids), compare_int64);

  Hashtable *right_counts_ht = hashtable_new();
  assert(right_counts_ht != NULL && "hashtable_new failed");
  int64_t total_distance = 0;

  for (size_t i = 0; i < num_ids; i++) {
    int64_t right = right_ids[i];
    int64_t right_count = 1;

    char *key = get_key(right);
    Option existing_count = hashtable_get(right_counts_ht, key);
    if (existing_count.some) {
      right_count += (int64_t)existing_count.value;
    }

    hashtable_set(right_counts_ht, key, (void *)right_count);
    free(key);

    total_distance += llabs(left_ids[i] - right);
  }

  int64_t similarity_score = 0;
  for (size_t i = 0; i < num_ids; i++) {
    int64_t left = left_ids[i];

    char *key = get_key(left);
    Option right_count = hashtable_get(right_counts_ht, key);
    free(key);

    similarity_score +=
        left * (right_count.some ? (int64_t)right_count.value : 0);
  }

  hashtable_free(right_counts_ht);

  print_day(1, "Hystorian Hysteria");
  print_part(1, (uint64_t)total_distance, PART1_ANSWER);
  print_part(2, (uint64_t)similarity_score, PART2_ANSWER);

  return 0;
}
