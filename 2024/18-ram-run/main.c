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

#include "../lib/ansi.h"
#include "../lib/aoc.h"
#include "../lib/binary-heap.h"
#include "../lib/txt.h"

// #define USE_EXAMPLE

#ifdef USE_EXAMPLE
#define INPUT_FILENAME "example-input.txt"
#define MEMORY_SPACE_WIDTH 7
#define SIM_BYTES 12
#define PART1_ANSWER 22
#define PART2_ANSWER 2
#else
#define INPUT_FILENAME "puzzle-input.txt"
#define MEMORY_SPACE_WIDTH 71
#define SIM_BYTES 1024
#define PART1_ANSWER 374
#define PART2_ANSWER 2
#endif

typedef enum {
  BYTE_STATE_SAFE,
  BYTE_STATE_CORRUPTED,
  BYTE_STATE_PATH
} ByteState;

const char BYTE_CHAR[] = {[BYTE_STATE_SAFE] = '.',
                          [BYTE_STATE_CORRUPTED] = '#',
                          [BYTE_STATE_PATH] = 'O'};

typedef struct ByteNode {
  ByteState state;
  int64_t row, col;
  bool seen;
  int64_t cost;
  struct ByteNode *from;
} ByteNode;

typedef struct {
  ByteState **bytes;
  int64_t width;
} MemorySpace;

void memory_space_print(MemorySpace *memory_space) {
  for (int64_t r = 0; r < memory_space->width; r++) {
    for (int64_t c = 0; c < memory_space->width; c++) {
      ByteState byte = memory_space->bytes[r][c];
      if (byte == BYTE_STATE_CORRUPTED) {
        ansi_esc(ANSI_CODE_FG_RED);
      } else if (byte == BYTE_STATE_PATH) {
        ansi_esc(ANSI_CODE_FG_GREEN);
      }
      putchar(BYTE_CHAR[byte]);
      ansi_reset();
    }
    putchar('\n');
  }
  putchar('\n');
}

void memory_space_free(MemorySpace *memory_space) {
  for (int64_t i = 0; i < memory_space->width; i++) {
    free(memory_space->bytes[i]);
  }
  free(memory_space->bytes);
}

MemorySpace memory_space_parse(Txt *txt, int64_t memory_width,
                               uint64_t sim_bytes) {
  MemorySpace memory_space = {.width = memory_width};

  memory_space.bytes =
      malloc(sizeof(*memory_space.bytes) * (size_t)memory_space.width);
  assert(memory_space.bytes != NULL && "malloc failed");

  for (int64_t r = 0; r < memory_space.width; r++) {
    ByteState *row = malloc(sizeof(*row) * (size_t)memory_space.width);
    assert(row != NULL && "malloc failed");
    for (int64_t c = 0; c < memory_space.width; c++) {
      row[c] = BYTE_STATE_SAFE;
    }
    memory_space.bytes[r] = row;
  }

  assert(txt->num_lines >= sim_bytes && "invalid puzzle input");

  for (uint64_t i = 0; i < sim_bytes; i++) {
    size_t col, row;
    int matched = sscanf(txt->lines[i], "%zu,%zu", &col, &row);
    assert(matched == 2 && "sscanf failed");

    memory_space.bytes[row][col] = BYTE_STATE_CORRUPTED;
  }

  return memory_space;
}

int64_t compare_byte_nodes(const void *a, const void *b) {
  int64_t a_cost = (*(ByteNode *)a).cost;
  int64_t b_cost = (*(ByteNode *)b).cost;

  // Reverse comparison for minheap
  return b_cost - a_cost;
}

int64_t steps_to_exit(MemorySpace *memory_space) {
  BinaryHeap *priorityq = binaryheap_new(compare_byte_nodes);

  ByteNode nodes[memory_space->width][memory_space->width];
  for (int64_t r = 0; r < memory_space->width; r++) {
    for (int64_t c = 0; c < memory_space->width; c++) {
      nodes[r][c] = (ByteNode){.state = memory_space->bytes[r][c],
                               .row = r,
                               .col = c,
                               .seen = false,
                               .cost = -1,
                               .from = NULL};
    }
  }

  ByteNode *start = &nodes[0][0];
  start->seen = true;
  start->cost = 0;
  binaryheap_push(priorityq, start);

  while (!binaryheap_is_empty(priorityq)) {
    Option node_option = binaryheap_pop(priorityq);
    assert(node_option.some);
    ByteNode *curr_node = node_option.value;

    if (curr_node->row == memory_space->width - 1 &&
        curr_node->col == memory_space->width - 1) {

      ByteNode *path_node = curr_node;
      while (path_node != NULL) {
        memory_space->bytes[path_node->row][path_node->col] = BYTE_STATE_PATH;
        path_node = path_node->from;
      }

      return curr_node->cost;
    }

    struct Coord {
      int64_t row, col;
    } next[] = {
        {.row = curr_node->row - 1, .col = curr_node->col},
        {.row = curr_node->row + 1, .col = curr_node->col},
        {.row = curr_node->row, .col = curr_node->col - 1},
        {.row = curr_node->row, .col = curr_node->col + 1},
    };

    for (size_t i = 0; i < (sizeof(next) / sizeof(next[0])); i++) {
      struct Coord next_coord = next[i];

      bool is_valid_row =
          next_coord.row >= 0 && next_coord.row < memory_space->width;
      bool is_valid_col =
          next_coord.col >= 0 && next_coord.col < memory_space->width;
      if (!(is_valid_row && is_valid_col)) {
        continue;
      }

      ByteNode *byte_node = &nodes[next_coord.row][next_coord.col];
      if (!byte_node->seen && byte_node->state != BYTE_STATE_CORRUPTED) {
        byte_node->cost = curr_node->cost + 1;
        byte_node->seen = true;
        byte_node->from = curr_node;
        binaryheap_push(priorityq, byte_node);
      }
    }
  }

  fprintf(stderr, "Exit not found.\n");
  exit(EXIT_FAILURE);
}

int main(void) {
  Txt *txt = txt_read_file(INPUT_FILENAME);
  MemorySpace memory_space =
      memory_space_parse(txt, MEMORY_SPACE_WIDTH, SIM_BYTES);

  int64_t steps = steps_to_exit(&memory_space);
  memory_space_print(&memory_space);

  memory_space_free(&memory_space);
  txt_free(txt);

  print_day(18, "RAM Run");
  print_part_uint64(1, (uint64_t)steps, PART1_ANSWER);
  // print_part_uint64(2, 0, PART2_ANSWER);

  return 0;
}
