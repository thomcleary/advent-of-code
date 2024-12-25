/*
Day 20: Race Condition
https://adventofcode.com/2024/day/20
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

#include "../lib/aoc.h"
#include "../lib/txt.h"

// #define USE_EXAMPLE

#ifdef USE_EXAMPLE
#define INPUT_FILENAME "example-input.txt"
#define PART1_MIN_TIME_SAVED 2
#define PART1_ANSWER 44
#define PART2_MIN_TIME_SAVED 76
#define PART2_ANSWER 3
#else
#define INPUT_FILENAME "puzzle-input.txt"
#define PART1_MIN_TIME_SAVED 100
#define PART1_ANSWER 1378
#define PART2_MIN_TIME_SAVED 100
#define PART2_ANSWER 975379
#endif

#define WALL '#'
#define PATH '.'
#define START 'S'
#define END 'E'

typedef struct {
  int64_t row, column;
} Coord;

typedef struct {
  bool is_path;
  Coord from;
  int64_t steps_to_end;
} CodePathTile;

typedef struct {
  CodePathTile **path;
  int64_t rows, columns;
  Coord start, end;
} CodePath;

bool coord_equal(Coord *a, Coord *b) {
  return a->row == b->row && a->column == b->column;
}

void codepath_free(CodePath *code_path) {
  for (int64_t i = 0; i < code_path->rows; i++) {
    free(code_path->path[i]);
  }
  free(code_path->path);
}

int64_t codepath_walk(CodePath *code_path, Coord *position) {
  if (coord_equal(&code_path->end, position)) {
    int64_t steps_to_end = 0;
    code_path->path[position->row][position->column].steps_to_end =
        steps_to_end;
    return steps_to_end + 1;
  }

  CodePathTile *curr_tile = &code_path->path[position->row][position->column];

  const Coord directions[] = {
      {.row = -1, .column = 0},
      {.row = 1, .column = 0},
      {.row = 0, .column = -1},
      {.row = 0, .column = 1},
  };

  Coord *next_pos = NULL;
  CodePathTile *next_tile = NULL;

  for (size_t i = 0; i < (sizeof(directions) / sizeof(directions[0])); i++) {
    Coord direction = directions[i];
    Coord neighbour = {.row = position->row + direction.row,
                       .column = position->column + direction.column};
    CodePathTile *neighbour_tile =
        &code_path->path[neighbour.row][neighbour.column];

    if (neighbour_tile->is_path && !coord_equal(&neighbour, &curr_tile->from)) {
      next_pos = &neighbour;
      next_tile = neighbour_tile;
      next_tile->from = *position;
      break;
    }
  }
  assert(next_pos != NULL && "next position not found");
  assert(next_tile != NULL && "next tile not found");

  curr_tile->steps_to_end = codepath_walk(code_path, next_pos);

  return curr_tile->steps_to_end + 1;
}

CodePath codepath_parse(Txt *txt) {
  assert(txt->num_lines > 0 && "empty puzzle input");

  CodePath code_path = {.rows = (int64_t)txt->num_lines,
                        .columns = (int64_t)strlen(txt->lines[0])};

  // https://github.com/llvm/llvm-project/issues/78558
  // NOLINTNEXTLINE(bugprone-sizeof-expression)
  code_path.path = malloc(sizeof(*code_path.path) * (size_t)(code_path.rows));
  assert(code_path.path != NULL && "malloc failed");

  for (int64_t r = 0; r < code_path.rows; r++) {
    code_path.path[r] =
        malloc(sizeof(*code_path.path[r]) * (size_t)code_path.columns);
    assert(code_path.path[r] != NULL && "malloc failed");

    char *line = txt->lines[r];
    for (int64_t c = 0; c < code_path.columns; c++) {
      char tile = line[c];
      code_path.path[r][c] = (CodePathTile){.is_path = tile != WALL,
                                            .from = {.row = -1, .column = -1},
                                            .steps_to_end = -1};

      if (tile == START) {
        code_path.start = (Coord){.row = r, .column = c};
      } else if (tile == END) {
        code_path.end = (Coord){.row = r, .column = c};
      }
    }
  }

  codepath_walk(&code_path, &code_path.start);

  return code_path;
}

uint64_t find_cheats(CodePath *code_path, int64_t min_time_saved,
                     int64_t cheat_length) {
  int64_t path_length =
      code_path->path[code_path->start.row][code_path->start.column]
          .steps_to_end;

  uint64_t cheats_found = 0;

  for (int64_t row = 0; row < code_path->rows; row++) {
    for (int64_t col = 0; col < code_path->columns; col++) {
      for (int64_t steps = 2; steps <= cheat_length; steps++) {
        Coord start_position = {.row = row, .column = col};
        CodePathTile start_tile =
            code_path->path[start_position.row][start_position.column];

        for (int64_t x = -steps; x <= steps; x++) {
          int64_t y = steps - (x < 0 ? -x : x);

          const Coord end_directions[] = {
              {.row = x, .column = y},
              {.row = x, .column = -y},
          };
          size_t num_directions = y == 0 ? 1 : 2;

          for (size_t i = 0; i < num_directions; i++) {
            Coord direction = end_directions[i];
            Coord end_position = {.row = start_position.row + direction.row,
                                  .column =
                                      start_position.column + direction.column};

            bool valid_row =
                end_position.row > 0 && end_position.row < code_path->rows - 1;
            bool valid_column = end_position.column > 0 &&
                                end_position.column < code_path->columns - 1;

            if (!valid_row || !valid_column) {
              continue;
            }

            CodePathTile *end_tile =
                &code_path->path[end_position.row][end_position.column];

            if (!end_tile->is_path) {
              continue;
            }

            int64_t steps_to_tile = path_length - start_tile.steps_to_end;
            int64_t steps_to_end_tile = steps_to_tile + steps;
            int64_t cheat_path_length =
                steps_to_end_tile + end_tile->steps_to_end;

            if (path_length - cheat_path_length >= min_time_saved) {
              cheats_found++;
            }
          }
        }
      }
    }
  }

  return cheats_found;
}

int main(void) {
  Txt *txt = txt_read_file(INPUT_FILENAME);

  CodePath code_path = codepath_parse(txt);

  uint64_t num_deprecated_cheats =
      find_cheats(&code_path, PART1_MIN_TIME_SAVED, 2);

  uint64_t num_cheats = find_cheats(&code_path, PART2_MIN_TIME_SAVED, 20);

  codepath_free(&code_path);
  txt_free(txt);

  print_day(20, "Race Condition");
  print_part_uint64(1, num_deprecated_cheats, PART1_ANSWER);
  print_part_uint64(2, num_cheats, PART2_ANSWER);

  return 0;
}
