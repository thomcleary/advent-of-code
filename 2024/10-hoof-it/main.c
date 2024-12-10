/*
Day 10: Hoof It
https://adventofcode.com/2024/day/10
*/

// #define USE_EXAMPLE
#define _DEFAULT_SOURCE

#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../lib/ansi.h"
#include "../lib/aoc.h"
#include "../lib/txt.h"
#include "main.h"

typedef struct Coord {
  size_t row, column;
} Coord;

typedef struct TopographicMap {
  uint8_t *positions;
  size_t num_rows, num_columns;

  Coord *trailheads;
  size_t num_trailheads;
} TopographicMap;

const AnsiCode HEIGHT_BG_COLOUR_CODE[10] = {ANSI_CODE_BG_BLACK,
                                            ANSI_CODE_BG_BLUE,
                                            ANSI_CODE_BG_BRIGHT_BLUE,
                                            ANSI_CODE_BG_MAGENTA,
                                            ANSI_CODE_BG_BRIGHT_MAGENTA,
                                            ANSI_CODE_BG_RED,
                                            ANSI_CODE_BG_BRIGHT_RED,
                                            ANSI_CODE_BG_YELLOW,
                                            ANSI_CODE_BG_BRIGHT_YELLOW,
                                            ANSI_CODE_BG_GREEN

};

void topographic_map_free(TopographicMap *map) {
  free(map->positions);
  free(map->trailheads);
  free(map);
}

void topographic_map_print(TopographicMap *map) {
  printf("Map (%zu x %zu)\n", map->num_rows, map->num_columns);
  for (size_t i = 0; i < map->num_rows; i++) {
    for (size_t j = 0; j < map->num_columns; j++) {
      uint8_t pos = map->positions[(i * map->num_columns) + j];
      ansi_esc(pos == 0 ? ANSI_CODE_FAINT : ANSI_CODE_FG_BLACK);
      ansi_esc(HEIGHT_BG_COLOUR_CODE[pos]);
      printf("%d", pos);
      ansi_reset();
    }
    printf("\n");
  }
  printf("\n");

  printf("Trailheads (%zu)\n", map->num_trailheads);
  for (size_t i = 0; i < map->num_trailheads; i++) {
    Coord trailhead = map->trailheads[i];
    printf("(%zu, %zu)\n", trailhead.row, trailhead.column);
  }
  printf("\n");
}

TopographicMap *topographic_map_parse(Txt *txt) {
  TopographicMap *map = malloc(sizeof(*map));
  assert(map != NULL && "malloc failed");

  map->num_rows = txt->num_lines;
  assert(map->num_rows > 0 && "invalid row count");

  map->num_columns = strlen(txt->lines[0]);
  assert(map->num_columns > 0 && "invalid column count");

  map->positions =
      malloc(sizeof(*map->positions) * map->num_rows * map->num_columns);
  assert(map->positions != NULL && "malloc failed");

  for (size_t i = 0; i < map->num_rows; i++) {
    char *line = txt->lines[i];
    for (size_t j = 0; j < map->num_columns; j++) {
      map->positions[(i * map->num_columns) + j] = (uint8_t)line[j] - '0';
    }
  }

  map->num_trailheads = 0;
  map->trailheads =
      malloc(sizeof(*map->trailheads) * map->num_rows * map->num_columns);
  assert(map->trailheads != NULL && "malloc failed");

  for (size_t i = 0; i < map->num_rows; i++) {
    for (size_t j = 0; j < map->num_columns; j++) {
      uint8_t pos = map->positions[(i * map->num_columns) + j];
      if (pos == 0) {
        map->trailheads[map->num_trailheads++] = (Coord){.row = i, .column = j};
      }
    }
  }

  return map;
}

uint8_t topographic_map_get_height_at(TopographicMap *map, Coord position) {
  assert(position.row < map->num_rows && "invalid position row");
  assert(position.column < map->num_columns && "invalid position column");

  return map->positions[(position.row * map->num_columns) + position.column];
}

uint64_t topographic_map_trailhead_score(TopographicMap *map, Coord trailhead) {
  Coord *stack = malloc(sizeof(*stack) * map->num_rows * map->num_columns + 1);
  assert(stack != NULL && "malloc failed");

  bool *visited = calloc(map->num_rows * map->num_columns, sizeof(*stack));
  assert(visited != NULL && "malloc failed");

  size_t stack_pointer = 1;
  stack[stack_pointer] = trailhead;

  uint64_t trailhead_score = 0;

  while (stack_pointer > 0) {
    Coord popped = stack[stack_pointer--];
    size_t visited_index = (popped.row * map->num_columns) + popped.column;

    if (visited[visited_index]) {
      continue;
    } else {
      visited[(popped.row * map->num_columns) + popped.column] = true;
    }

    if (topographic_map_get_height_at(map, popped) == 9) {
      trailhead_score++;
      continue;
    }

    uint8_t popped_height = topographic_map_get_height_at(map, popped);

    if (popped.row < map->num_rows) {
      if (popped.row > 0) {
        Coord up = {.row = popped.row - 1, .column = popped.column};
        bool seen = visited[(up.row * map->num_columns) + up.column];
        uint8_t up_height = topographic_map_get_height_at(map, up);

        if (!seen && (popped_height == up_height - 1)) {
          stack[++stack_pointer] = up;
        }
      }

      if (popped.row < map->num_rows - 1) {
        Coord down = {.row = popped.row + 1, .column = popped.column};
        bool seen = visited[(down.row * map->num_columns) + down.column];
        uint8_t down_height = topographic_map_get_height_at(map, down);

        if (!seen && (popped_height == down_height - 1)) {
          stack[++stack_pointer] = down;
        }
      }
    }

    if (popped.row < map->num_columns) {
      if (popped.column > 0) {
        Coord left = {.row = popped.row, .column = popped.column - 1};
        bool seen = visited[(left.row * map->num_columns) + left.column];
        uint8_t left_height = topographic_map_get_height_at(map, left);

        if (!seen && (popped_height == left_height - 1)) {
          stack[++stack_pointer] = left;
        }
      }

      if (popped.column < map->num_columns - 1) {
        Coord right = {.row = popped.row, .column = popped.column + 1};
        bool seen = visited[(right.row * map->num_columns) + right.column];
        uint8_t right_height = topographic_map_get_height_at(map, right);

        if (!seen && (popped_height == right_height - 1)) {
          stack[++stack_pointer] = right;
        }
      }
    }
  }

  free(stack);
  free(visited);

  return trailhead_score;
}

int main(void) {
  Txt *txt = txt_read(stdin);
  TopographicMap *map = topographic_map_parse(txt);
  // topographic_map_print(map);

  uint64_t trailhead_score_total = 0;
  for (size_t i = 0; i < map->num_trailheads; i++) {
    trailhead_score_total +=
        topographic_map_trailhead_score(map, map->trailheads[i]);
  }

  topographic_map_free(map);
  txt_free(txt);

  print_day(10, "Hoof It");
  print_part(1, trailhead_score_total, PART1_ANSWER);
  print_part(2, 0, PART2_ANSWER);

  return 0;
}
