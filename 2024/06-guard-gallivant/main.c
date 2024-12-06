/*
Day 6: Guard Gallivant
https://adventofcode.com/2024/day/6
*/

#include <stddef.h>
#define USE_EXAMPLE
#define _DEFAULT_SOURCE

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../lib/aoc.h"
#include "../lib/txt.h"
#include "main.h"

typedef enum Direction {
  DIRECTION_UP,
  DIRECTION_DOWN,
  DIRECTION_LEFT,
  DIRECTION_RIGHT,
} Direction;

typedef enum Position {
  POSITION_UNVISITED,
  POSITION_VISITED,
  POSITION_OBSTRUCTION,
} Position;

typedef struct Coord {
  size_t row, column;
} Coord;

typedef struct LabMap {
  Position **positions;
  size_t rows, columns;
  Coord guard_start;
} LabMap;

void labmap_free(LabMap *map) {
  for (size_t i = 0; i < map->rows; i++) {
    free(map->positions[i]);
  }
  free(map->positions);
  free(map);
}

void labmap_print(LabMap *map) {
  for (size_t r = 0; r < map->rows; r++) {
    for (size_t c = 0; c < map->columns; c++) {
      Position pos = map->positions[r][c];
      char pos_ch;
      if (pos == POSITION_VISITED) {
        pos_ch = 'X';
      } else if (map->guard_start.row == r && map->guard_start.column == c) {
        pos_ch = '^';
      } else {
        pos_ch = pos == POSITION_UNVISITED ? '.' : '#';
      }
      putchar(pos_ch);
    }
    printf("\n");
  }
  printf("\n");
}

LabMap *labmap_parse(Txt *txt) {
  LabMap *map = malloc(sizeof(*map));
  assert(map != NULL && "malloc failed");
  map->rows = txt->num_lines;
  map->columns = strlen(txt->lines[0]);
  map->positions = malloc(sizeof(*map->positions) * map->rows);
  assert(map->positions != NULL && "malloc failed");

  for (size_t i = 0; i < map->rows; i++) {
    map->positions[i] = malloc(sizeof(map->positions[i]) * map->columns);
    assert(map->positions[i] != NULL && "malloc failed");
  }

  for (size_t r = 0; r < txt->num_lines; r++) {
    Position *row = map->positions[r];
    char *line = txt->lines[r];

    for (size_t c = 0; c < map->columns; c++) {
      char ch = line[c];
      assert((ch == '^' || ch == '.' || ch == '#') &&
             "invalid position character");

      Position pos;
      if (ch == '^') {
        pos = POSITION_UNVISITED;
        map->guard_start = (Coord){.row = r, .column = c};
      } else {
        pos = ch == '.' ? POSITION_UNVISITED : POSITION_OBSTRUCTION;
      }
      row[c] = pos;
    }
  }

  return map;
}

bool is_guard_exiting(LabMap *map, Coord coord, Direction direction) {
  assert(coord.row < map->rows && "coord.row is outside map");
  assert(coord.column < map->columns && "coord.column is outside map");

  switch (direction) {
  case DIRECTION_UP:
    return coord.row == 0;
  case DIRECTION_DOWN:
    return coord.row == (map->rows - 1);
  case DIRECTION_LEFT:
    return coord.column == 0;
  case DIRECTION_RIGHT:
    return coord.column == (map->columns - 1);
  default:
    fprintf(stderr, "Invalid Direction [%d]\n", direction);
    exit(EXIT_FAILURE);
  }
}

Coord next_coord(Coord guard_coord, Direction direction) {
  switch (direction) {
  case DIRECTION_UP:
    guard_coord.row--;
    break;
  case DIRECTION_DOWN:
    guard_coord.row++;
    break;
  case DIRECTION_LEFT:
    guard_coord.column--;
    break;
  case DIRECTION_RIGHT:
    guard_coord.column++;
  }
  return guard_coord;
}

Direction turn_right(Direction direction) {
  switch (direction) {
  case DIRECTION_UP:
    return DIRECTION_RIGHT;
  case DIRECTION_RIGHT:
    return DIRECTION_DOWN;
  case DIRECTION_DOWN:
    return DIRECTION_LEFT;
  case DIRECTION_LEFT:
    return DIRECTION_UP;
  default:
    fprintf(stderr, "Invalid Direction [%d]\n", direction);
    exit(EXIT_FAILURE);
  }
}

void labmap_predict(LabMap *map) {
  Coord guard_coord = map->guard_start;
  Direction current_dir = DIRECTION_UP;

  while (!is_guard_exiting(map, guard_coord, current_dir)) {
    map->positions[guard_coord.row][guard_coord.column] = POSITION_VISITED;
    Coord next_guard_coord = next_coord(guard_coord, current_dir);
    Position next_pos =
        map->positions[next_guard_coord.row][next_guard_coord.column];

    if (next_pos == POSITION_OBSTRUCTION) {
      current_dir = turn_right(current_dir);
    } else {
      guard_coord = next_guard_coord;
    }
  }

  map->positions[guard_coord.row][guard_coord.column] = POSITION_VISITED;
}

size_t labmap_positions_visited(LabMap *map) {
  size_t count = 0;
  for (size_t r = 0; r < map->rows; r++) {
    for (size_t c = 0; c < map->columns; c++) {
      if (map->positions[r][c] == POSITION_VISITED) {
        count++;
      }
    }
  }
  return count;
}

int main(void) {
  Txt *txt = txt_read(stdin);
  LabMap *map = labmap_parse(txt);

  // labmap_print(map);
  labmap_predict(map);
  // labmap_print(map);
  size_t positions_visited = labmap_positions_visited(map);

  labmap_free(map);
  txt_free(txt);

  print_day(6, "Guard Gallivant");
  printf("Part 1: %zu\n", positions_visited);
  printf("Part 2: TODO\n");

  assert(positions_visited == PART1_ANSWER);
  // assert(0 == PART2_ANSWER);

  return 0;
}
