/*
Day 10: Hoof It
https://adventofcode.com/2024/day/10
*/

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

// #define USE_EXAMPLE

#ifdef USE_EXAMPLE
#define PART1_ANSWER 36
#define PART2_ANSWER 81
#else
#define PART1_ANSWER 574
#define PART2_ANSWER 1238
#endif

typedef struct Coord {
  size_t row, column;
} Coord;

typedef struct TopographicMap {
  uint8_t *positions;
  size_t num_rows, num_columns;

  Coord *trailheads;
  size_t num_trailheads;
} TopographicMap;

typedef struct TrailheadInfo {
  uint64_t peaks_reachable, num_hiking_trails;
} TrailheadInfo;

const AnsiCode HEIGHT_BG_COLOUR_CODE[10] = {
    ANSI_CODE_BG_BLACK,   ANSI_CODE_BG_BLUE,    ANSI_CODE_BG_BLUE,
    ANSI_CODE_BG_MAGENTA, ANSI_CODE_BG_MAGENTA, ANSI_CODE_BG_RED,
    ANSI_CODE_BG_RED,     ANSI_CODE_BG_YELLOW,  ANSI_CODE_BG_YELLOW,
    ANSI_CODE_BG_GREEN

};

void topographic_map_free(TopographicMap *map) {
  free(map->positions);
  free(map->trailheads);
  free(map);
}

void topographic_map_print(TopographicMap *map) {
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

TrailheadInfo topographic_map_trailhead_info(TopographicMap *map,
                                             Coord trailhead) {
  Coord *stack = malloc(sizeof(*stack) * map->num_rows * map->num_columns + 1);
  assert(stack != NULL && "malloc failed");

  bool *visited = calloc(map->num_rows * map->num_columns, sizeof(*stack));
  assert(visited != NULL && "malloc failed");

  size_t stack_pointer = 1;
  stack[stack_pointer] = trailhead;

  TrailheadInfo info = {.peaks_reachable = 0, .num_hiking_trails = 0};

  while (stack_pointer > 0) {
    Coord popped = stack[stack_pointer--];

    size_t visited_index = (popped.row * map->num_columns) + popped.column;
    bool popped_seen = visited[visited_index];
    visited[visited_index] = true;

    if (topographic_map_get_height_at(map, popped) == 9) {
      if (!popped_seen) {
        info.peaks_reachable++;
      }
      info.num_hiking_trails++;
      continue;
    }

    uint8_t popped_height = topographic_map_get_height_at(map, popped);

    if (popped.row < map->num_rows) {
      if (popped.row > 0) {
        Coord up = {.row = popped.row - 1, .column = popped.column};
        if (popped_height == topographic_map_get_height_at(map, up) - 1) {
          stack[++stack_pointer] = up;
        }
      }

      if (popped.row < map->num_rows - 1) {
        Coord down = {.row = popped.row + 1, .column = popped.column};
        if (popped_height == topographic_map_get_height_at(map, down) - 1) {
          stack[++stack_pointer] = down;
        }
      }
    }

    if (popped.row < map->num_columns) {
      if (popped.column > 0) {
        Coord left = {.row = popped.row, .column = popped.column - 1};
        if (popped_height == topographic_map_get_height_at(map, left) - 1) {
          stack[++stack_pointer] = left;
        }
      }

      if (popped.column < map->num_columns - 1) {
        Coord right = {.row = popped.row, .column = popped.column + 1};
        if (popped_height == topographic_map_get_height_at(map, right) - 1) {
          stack[++stack_pointer] = right;
        }
      }
    }
  }

  free(stack);
  free(visited);

  return info;
}

int main(void) {
  Txt *txt = txt_read(stdin);
  TopographicMap *map = topographic_map_parse(txt);
  // topographic_map_print(map);

  TrailheadInfo totals = {.peaks_reachable = 0, .num_hiking_trails = 0};

  for (size_t i = 0; i < map->num_trailheads; i++) {
    TrailheadInfo info =
        topographic_map_trailhead_info(map, map->trailheads[i]);
    totals.peaks_reachable += info.peaks_reachable;
    totals.num_hiking_trails += info.num_hiking_trails;
  }

  topographic_map_free(map);
  txt_free(txt);

  print_day(10, "Hoof It");
  print_part(1, totals.peaks_reachable, PART1_ANSWER);
  print_part(2, totals.num_hiking_trails, PART2_ANSWER);

  return 0;
}
