/*
Day 6: Guard Gallivant
https://adventofcode.com/2024/day/6
*/

#define _DEFAULT_SOURCE
#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../lib/aoc.h"
#include "../lib/txt.h"

// #define USE_EXAMPLE

#ifdef USE_EXAMPLE
#define PART1_ANSWER 41
#define PART2_ANSWER 6
#else
#define PART1_ANSWER 4883
#define PART2_ANSWER 1655
#endif

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
  uint64_t row, column;
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

void labmap_setpos(LabMap *map, Coord coord, Position pos) {
  map->positions[coord.row][coord.column] = pos;
}

void labmap_reset(LabMap *map) {
  for (size_t r = 0; r < map->rows; r++) {
    for (size_t c = 0; c < map->columns; c++) {
      Position pos = map->positions[r][c];
      if (pos == POSITION_VISITED) {
        labmap_setpos(map, (Coord){.row = r, .column = c}, POSITION_UNVISITED);
      }
    }
  }
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

bool labmap_predict(LabMap *map) {
  const uint8_t num_directions = 4;

  uint8_t *visits = calloc((map->rows * map->columns), sizeof(*visits));
  assert(visits != NULL && "calloc failed");

  Coord guard_coord = map->guard_start;
  Direction current_dir = DIRECTION_UP;

  while (!is_guard_exiting(map, guard_coord, current_dir)) {
    labmap_setpos(map, guard_coord, POSITION_VISITED);

    size_t visits_index = guard_coord.row * map->columns + guard_coord.column;

    if (visits[visits_index] > num_directions) {
      free(visits);
      return true;
    }

    Coord next_guard_coord = next_coord(guard_coord, current_dir);
    Position next_pos =
        map->positions[next_guard_coord.row][next_guard_coord.column];

    if (next_pos == POSITION_OBSTRUCTION) {
      current_dir = turn_right(current_dir);
    } else {
      guard_coord = next_guard_coord;
      visits[visits_index] += 1;
    }
  }

  labmap_setpos(map, guard_coord, POSITION_VISITED);

  free(visits);
  return false; // No loop, guard exited
}

uint64_t labmap_positions_visited(LabMap *map) {
  uint64_t count = 0;
  for (size_t r = 0; r < map->rows; r++) {
    for (size_t c = 0; c < map->columns; c++) {
      if (map->positions[r][c] == POSITION_VISITED) {
        count++;
      }
    }
  }
  return count;
}

uint64_t labmap_positions_loopable(LabMap *map, uint64_t positions_visited) {
  Coord obstruction_coords[positions_visited];
  uint64_t visited = 0;

  for (size_t r = 0; r < map->rows; r++) {
    for (size_t c = 0; c < map->columns; c++) {
      bool is_start = map->guard_start.row == r && map->guard_start.column == c;
      if (!is_start && map->positions[r][c] == POSITION_VISITED) {
        obstruction_coords[visited++] = (Coord){.row = r, .column = c};
      }
    }
  }
  assert(visited == positions_visited - 1);

  uint64_t count = 0;

  for (size_t i = 0; i < visited; i++) {
    Coord obstruction_coord = obstruction_coords[i];
    labmap_reset(map);
    labmap_setpos(map, obstruction_coord, POSITION_OBSTRUCTION);

    if (labmap_predict(map)) {
      count++;
    }

    labmap_setpos(map, obstruction_coord, POSITION_UNVISITED);
  }

  return count;
}

int main(void) {
  Txt *txt = txt_read(stdin);
  LabMap *map = labmap_parse(txt);

  labmap_predict(map);
  uint64_t positions_visited = labmap_positions_visited(map);
  uint64_t positions_loopable =
      labmap_positions_loopable(map, positions_visited);

  labmap_free(map);
  txt_free(txt);

  print_day(6, "Guard Gallivant");
  print_part(1, positions_visited, PART1_ANSWER);
  print_part(2, positions_loopable, PART2_ANSWER);

  return 0;
}
