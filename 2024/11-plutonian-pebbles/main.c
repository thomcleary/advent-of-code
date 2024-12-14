/*
Day 11: Plutonian Pebbles
https://adventofcode.com/2024/day/11
*/

#define _DEFAULT_SOURCE
#include <assert.h>
#include <errno.h>
#include <inttypes.h>
#include <math.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../lib/aoc.h"
#include "../lib/hashtable.h"
#include "../lib/txt.h"

// #define USE_EXAMPLE

#ifdef USE_EXAMPLE
#define PART1_ANSWER 55312
#define PART2_ANSWER 65601038650482
#else
#define PART1_ANSWER 199986
#define PART2_ANSWER 236804088748754
#endif

const uint8_t UINT64_MAX_STRLEN = 20;
const uint8_t UINT8_MAX_STRLEN = 3;

typedef struct Stones {
  uint64_t *value;
  size_t length;
  size_t size;
} Stones;

void stones_free(Stones *stones) {
  free(stones->value);
  free(stones);
}

Stones *stones_parse(Txt *txt) {
  Stones *stones = malloc(sizeof(*stones));
  assert(stones != NULL && "malloc failed");

  stones->size = BUFSIZ;
  stones->length = 0;
  stones->value = malloc(sizeof(*stones->value) * stones->size);
  assert(stones->value != NULL && "malloc failed");

  assert(txt->num_lines == 1 && "invalid puzzle input");
  char *line = txt->lines[0];
  char *token;

  while ((token = strsep(&line, " ")) != NULL) {
    errno = 0;
    uint64_t stone = strtoull(token, NULL, 10);
    assert(errno == 0 && "strtol failed");

    if (stones->length == stones->size) {
      stones->size *= 2;
      stones->value =
          realloc(stones->value, sizeof(*stones->value) * stones->size);
      assert(stones->value != NULL && "realloc failed");
    }

    stones->value[stones->length++] = stone;
  }

  return stones;
}

uint64_t blink(uint64_t stone, uint8_t times, Hashtable *cache) {
  if (times == 0) {
    return 1;
  }

  char cache_key[UINT64_MAX_STRLEN + UINT8_MAX_STRLEN + 2]; // +1 ':', +1 '\0
  snprintf(cache_key, sizeof(cache_key), "%" PRIu64 ":%d", stone, times);

  HashtableGetResult cache_result = hashtable_get(cache, cache_key);
  if (cache_result.success) {
    return (uint64_t)cache_result.value;
  }

  uint64_t num_stone_digits = (uint64_t)log10((double)stone) + 1;
  uint64_t num_stones;

  if (stone == 0) {
    num_stones = blink(1, times - 1, cache);
  } else if (num_stone_digits % 2 == 0) {
    uint64_t split_by = (uint64_t)(pow(10, (double)num_stone_digits / 2));
    num_stones = blink(stone / split_by, times - 1, cache) +
                 blink(stone % split_by, times - 1, cache);
  } else {
    num_stones = blink(stone * 2024, times - 1, cache);
  }

  hashtable_set(cache, cache_key, (void *)num_stones);

  return num_stones;
}

uint64_t stones_blink(Stones *stones, uint8_t times) {
  Hashtable *cache = hashtable_new();

  uint64_t num_stones = 0;

  for (size_t i = 0; i < stones->length; i++) {
    num_stones += blink(stones->value[i], times, cache);
  }

  hashtable_free(cache);

  return num_stones;
}

int main(void) {
  Txt *txt = txt_read(stdin);
  Stones *stones = stones_parse(txt);

  uint64_t stones_after_25_blinks = stones_blink(stones, 25);
  uint64_t stones_after_75_blinks = stones_blink(stones, 75);

  stones_free(stones);
  txt_free(txt);

  print_day(11, "Plutonian Pebbles");
  print_part(1, stones_after_25_blinks, PART1_ANSWER);
  print_part(2, stones_after_75_blinks, PART2_ANSWER);

  return 0;
}
