/*
Day 11: Plutonian Pebbles
https://adventofcode.com/2024/day/11
*/

// #define USE_EXAMPLE
#define _DEFAULT_SOURCE

#include <assert.h>
#include <errno.h>
#include <inttypes.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../lib/aoc.h"
// #include "../lib/hashtable.h"
#include "../lib/txt.h"
#include "main.h"

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

uint64_t blink(uint64_t stone, uint8_t times) {
  if (times == 0) {
    return 1;
  }

  if (stone == 0) {
    return blink(1, times - 1);
  }

  const uint8_t int64_max_length = 20;
  char stone_str[int64_max_length + 1];
  snprintf(stone_str, sizeof(stone_str), "%" PRIu64, stone);
  size_t stone_str_length = strlen(stone_str);

  if (stone_str_length % 2 == 0) {
    size_t new_stone_str_length = (stone_str_length / 2);

    char left_stone_str[new_stone_str_length + 1];
    strncpy(left_stone_str, stone_str, new_stone_str_length);
    left_stone_str[new_stone_str_length] = '\0';

    char right_stone_str[new_stone_str_length + 1];
    strncpy(right_stone_str, stone_str + new_stone_str_length,
            new_stone_str_length);
    right_stone_str[new_stone_str_length] = '\0';

    errno = 0;
    uint64_t left_stone = strtoull(left_stone_str, NULL, 10);
    assert(errno == 0 && "strtoull failed");

    errno = 0;
    uint64_t right_stone = strtoull(right_stone_str, NULL, 10);
    assert(errno == 0 && "strtoull failed");

    return blink(left_stone, times - 1) + blink(right_stone, times - 1);
  }

  return blink(stone * 2024, times - 1);
}

uint64_t stones_blink(Stones *stones, uint8_t times) {
  // TODO: maybe use hashtable to cache blink counts of stones?
  // Hashtable *cache = hashtable_new();

  uint64_t num_stones = 0;

  for (size_t i = 0; i < stones->length; i++) {
    num_stones += blink(stones->value[i], times);
  }

  // hashtable_free(cache);

  return num_stones;
}

int main(void) {
  Txt *txt = txt_read(stdin);
  Stones *stones = stones_parse(txt);

  uint64_t num_stones = stones_blink(stones, 25);

  stones_free(stones);
  txt_free(txt);

  print_day(11, "Plutonian Pebbles");
  print_part(1, num_stones, PART1_ANSWER);
  // print_part(2, 2, PART2_ANSWER);

  return 0;
}
