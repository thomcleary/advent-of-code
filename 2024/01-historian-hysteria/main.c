/*
Day 1: Hystorian Hysteria
https://adventofcode.com/2024/day/1
*/

// #define USE_EXAMPLE
#define _DEFAULT_SOURCE

#include <assert.h>
#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "../lib/aoc.h"
#include "../lib/hashtable.h"
#include "main.h"

int get_ids(long left_ids[], long right_ids[]) {
  int num_lines = 0;
  char *line = NULL;
  size_t line_len = 0;
  errno = 0;

  while ((getline(&line, &line_len, stdin) != -1)) {
    long left, right;
    int matched = sscanf(line, "%ld %ld", &left, &right);
    assert(matched == 2 && "sscanf failed");

    left_ids[num_lines] = left;
    right_ids[num_lines] = right;
    num_lines++;
  }

  free(line);
  assert(errno == 0 && "getline failed");

  return num_lines;
}

int compare_int(const void *a, const void *b) {
  return *(int *)a - *(int *)b;
}

char *get_key(long id) {
  char *key = malloc(sizeof(*key) * 8); // All IDs are only 5 chars long
  assert(key != NULL && "malloc failed");

  int written = sprintf(key, "%ld", id);
  assert(written != -1 && "sprintf failed");

  return key;
}

void free_hashtable(struct hashtable *ht, long keys[], size_t num_keys) {
  for (int i = 0; i < num_keys; i++) {
    char *key = get_key(keys[i]);
    long *value = hashtable_get(ht, key);
    if (value != NULL) {
      free(value);
    }
  }

  hashtable_free(ht);
}

int main(void) {
  // There's only 1000 lines in the puzzle input, 1024 will do
  long left_ids[BUFSIZ], right_ids[BUFSIZ];
  int num_ids = get_ids(left_ids, right_ids);

  qsort(left_ids, num_ids, sizeof(*left_ids), compare_int);
  qsort(right_ids, num_ids, sizeof(*right_ids), compare_int);

  struct hashtable *right_counts_ht = hashtable_new();
  assert(right_counts_ht != NULL && "hashtable_new failed");
  long total_distance = 0;

  for (int i = 0; i < num_ids; i++) {
    long right = right_ids[i];

    char *key = get_key(right);
    long *right_count = hashtable_get(right_counts_ht, key);

    if (right_count == NULL) {
      right_count = malloc(sizeof(*right_count));
      assert(right_count != NULL && "malloc failed");
      *right_count = 1;
    } else {
      *right_count += 1;
    }

    hashtable_set(right_counts_ht, key, right_count);
    free(key);

    total_distance += labs(left_ids[i] - right);
  }

  long similarity_score = 0;
  for (int i = 0; i < num_ids; i++) {
    long left = left_ids[i];

    char *key = get_key(left);
    long *right_count = hashtable_get(right_counts_ht, key);
    free(key);

    similarity_score += left * (right_count == NULL ? 0 : *right_count);
  }

  free_hashtable(right_counts_ht, right_ids, num_ids);

  print_day(1, "Hystorian Hysteria");
  printf("Part 1: %ld\n", total_distance);
  printf("Part 2: %ld\n", similarity_score);

  assert(total_distance == PART1_ANSWER);
  assert(similarity_score == PART2_ANSWER);

  return 0;
}
