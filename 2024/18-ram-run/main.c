/*
Day 18: RAM Run
https://adventofcode.com/2024/day/18
*/

#define _DEFAULT_SOURCE
#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "../lib/aoc.h"
#include "../lib/binary-heap.h"
#include "../lib/txt.h"

// #define USE_EXAMPLE

#ifdef USE_EXAMPLE
#define INPUT_FILENAME "example-input.txt"
#define MEMORY_SPACE_WIDTH 7
#define SIM_BYTES 12
#define PART1_ANSWER 22
#define PART2_ANSWER "6,1"
#else
#define INPUT_FILENAME "puzzle-input.txt"
#define MEMORY_SPACE_WIDTH 71
#define SIM_BYTES 1024
#define PART1_ANSWER 374
#define PART2_ANSWER "30,12"
#endif

typedef enum {
  BYTE_STATE_SAFE,
  BYTE_STATE_CORRUPTED,
} ByteState;

const char BYTE_CHAR[] = {
    [BYTE_STATE_SAFE] = '.', [BYTE_STATE_CORRUPTED] = '#'};

typedef struct {
  int64_t row, col;
} Coord;

typedef struct ByteNode {
  ByteState state;
  Coord coord;
  bool seen;
  int64_t steps;
  struct ByteNode *from;
} ByteNode;

typedef struct {
  Coord *bytes;
  int64_t length;
} FallingBytes;

void falling_bytes_free(FallingBytes *falling_bytes) {
  free(falling_bytes->bytes);
}

FallingBytes falling_bytes_parse(Txt *txt) {
  FallingBytes falling_bytes = {
      .bytes = malloc(sizeof(*falling_bytes.bytes) * txt->num_lines),
      .length = (int64_t)txt->num_lines};
  assert(falling_bytes.bytes != NULL && "malloc failed");

  for (int64_t i = 0; i < falling_bytes.length; i++) {
    int64_t col, row;
    int matched = sscanf(txt->lines[i], "%" PRId64 ",%" PRId64, &col, &row);
    assert(matched == 2 && "sscanf failed");

    falling_bytes.bytes[i] = (Coord){.row = row, .col = col};
  }

  return falling_bytes;
}

int64_t compare_byte_nodes(const void *a, const void *b) {
  int64_t a_cost = (*(ByteNode *)a).steps;
  int64_t b_cost = (*(ByteNode *)b).steps;

  // Reverse comparison for minheap
  return b_cost - a_cost;
}

typedef struct {
  bool success;
  union {
    int64_t steps;
    Coord blocked_by_byte;
  } value;
} StepsToExitResult;

StepsToExitResult steps_to_exit(FallingBytes *falling_bytes,
                                int64_t memory_width, int64_t sim_bytes) {
  BinaryHeap *priorityq = binaryheap_new(compare_byte_nodes);

  ByteNode nodes[memory_width][memory_width];
  for (int64_t r = 0; r < memory_width; r++) {
    for (int64_t c = 0; c < memory_width; c++) {
      nodes[r][c] = (ByteNode){.state = BYTE_STATE_SAFE,
                               .coord = {.row = r, .col = c},
                               .seen = false,
                               .steps = -1,
                               .from = NULL};
    }
  }

  for (int64_t i = 0; i < sim_bytes; i++) {
    Coord falling_byte = falling_bytes->bytes[i];
    nodes[falling_byte.row][falling_byte.col].state = BYTE_STATE_CORRUPTED;
  }

  ByteNode *start = &nodes[0][0];
  start->seen = true;
  start->steps = 0;
  binaryheap_push(priorityq, start);

  while (!binaryheap_is_empty(priorityq)) {
    Option node_option = binaryheap_pop(priorityq);
    assert(node_option.some);
    ByteNode *curr_node = node_option.value;

    if (curr_node->coord.row == memory_width - 1 &&
        curr_node->coord.col == memory_width - 1) {
      return (StepsToExitResult){.success = true,
                                 .value.steps = curr_node->steps};
    }

    Coord next[] = {
        {.row = curr_node->coord.row - 1, .col = curr_node->coord.col},
        {.row = curr_node->coord.row + 1, .col = curr_node->coord.col},
        {.row = curr_node->coord.row, .col = curr_node->coord.col - 1},
        {.row = curr_node->coord.row, .col = curr_node->coord.col + 1},
    };

    for (size_t i = 0; i < (sizeof(next) / sizeof(next[0])); i++) {
      Coord next_coord = next[i];

      bool is_valid_row = next_coord.row >= 0 && next_coord.row < memory_width;
      bool is_valid_col = next_coord.col >= 0 && next_coord.col < memory_width;

      if (!(is_valid_row && is_valid_col)) {
        continue;
      }

      ByteNode *byte_node = &nodes[next_coord.row][next_coord.col];
      if (!byte_node->seen && byte_node->state == BYTE_STATE_SAFE) {
        byte_node->steps = curr_node->steps + 1;
        byte_node->seen = true;
        byte_node->from = curr_node;
        binaryheap_push(priorityq, byte_node);
      }
    }
  }

  return (StepsToExitResult){.success = false,
                             .value.blocked_by_byte =
                                 falling_bytes->bytes[sim_bytes - 1]};
}

Coord find_blocking_byte(FallingBytes *falling_bytes, int64_t memory_width,
                         int64_t sim_bytes) {
  StepsToExitResult result;
  while ((result = steps_to_exit(falling_bytes, memory_width, sim_bytes))
             .success) {
    sim_bytes++;
  }

  return result.value.blocked_by_byte;
}

int main(void) {
  Txt *txt = txt_read_file(INPUT_FILENAME);
  FallingBytes falling_bytes = falling_bytes_parse(txt);

  StepsToExitResult steps_result =
      steps_to_exit(&falling_bytes, MEMORY_SPACE_WIDTH, SIM_BYTES);
  assert(steps_result.success);

  Coord blocked_by_byte =
      find_blocking_byte(&falling_bytes, MEMORY_SPACE_WIDTH, SIM_BYTES);

  char byte_str[5 + 1]; // "xx,xx" + '\0'
  snprintf(byte_str, sizeof(byte_str), "%" PRId64 ",%" PRId64 "",
           blocked_by_byte.col, blocked_by_byte.row);

  falling_bytes_free(&falling_bytes);
  txt_free(txt);

  print_day(18, "RAM Run");
  print_part_uint64(1, (uint64_t)steps_result.value.steps, PART1_ANSWER);
  print_part_str(2, byte_str, PART2_ANSWER);

  return 0;
}
