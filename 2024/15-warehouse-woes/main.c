/*
Day 15: Warehouse Woes
https://adventofcode.com/2024/day/15
*/

#define _DEFAULT_SOURCE
#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "../lib/ansi.h"
#include "../lib/aoc.h"
#include "../lib/txt.h"

// #define USE_EXAMPLE
// #define VISUALISE

#ifdef USE_EXAMPLE
#define PART1_ANSWER 10092
#define PART2_ANSWER 2
#else
#define PART1_ANSWER 1465523
#define PART2_ANSWER 2
#endif

typedef enum {
  ROBOT_MOVE_UP,
  ROBOT_MOVE_DOWN,
  ROBOT_MOVE_LEFT,
  ROBOT_MOVE_RIGHT,
} RobotMove;

typedef enum { TILE_WALL, TILE_BOX, TILE_ROBOT, TILE_EMPTY } Tile;

typedef struct {
  size_t row, column;
} Coord;

typedef struct {
  Tile **map;
  size_t map_rows, map_columns;

  Coord robot_pos;

  RobotMove *robot_moves;
  size_t num_robot_moves;
  size_t current_move;
} Warehouse;

void warehouse_free(Warehouse *warehouse) {
  free(warehouse->map);
  free(warehouse->robot_moves);
  free(warehouse);
}

Warehouse *warehouse_parse(Txt *txt) {
  SplitTxt *split_txt = txt_split(txt, "");
  assert(split_txt->num_txts == 2 && "invalid puzzle input");
  Txt map_txt = split_txt->txts[0];
  Txt moves_txt = split_txt->txts[1];

  Warehouse *warehouse = malloc(sizeof(*warehouse));
  assert(warehouse != NULL && "malloc failed");

  warehouse->map_rows = map_txt.num_lines;
  warehouse->map_columns = strlen(map_txt.lines[0]);
  warehouse->map = malloc(sizeof(*warehouse->map) * warehouse->map_rows);
  assert(warehouse->map != NULL && "malloc failed");

  for (size_t i = 0; i < warehouse->map_rows; i++) {
    warehouse->map[i] =
        malloc((sizeof(warehouse->map[i]) * warehouse->map_columns));
    assert(warehouse->map[i] != NULL && "malloc failed");

    for (size_t j = 0; j < warehouse->map_columns; j++) {
      char ch = map_txt.lines[i][j];
      Tile tile;

      switch (ch) {
      case '#':
        tile = TILE_WALL;
        break;
      case 'O':
        tile = TILE_BOX;
        break;
      case '@':
        tile = TILE_ROBOT;
        warehouse->robot_pos = (Coord){.row = i, .column = j};
        break;
      case '.':
        tile = TILE_EMPTY;
        break;
      default:
        fprintf(stderr, "Invalid map tile [%c]\n", ch);
        exit(EXIT_FAILURE);
      }

      warehouse->map[i][j] = tile;
    }
  }

  size_t move_txt_line_length = strlen(moves_txt.lines[0]);
  size_t robot_moves_size = moves_txt.num_lines * move_txt_line_length;
  warehouse->num_robot_moves = 0;
  warehouse->current_move = 0;
  warehouse->robot_moves =
      malloc(sizeof(*warehouse->robot_moves) * robot_moves_size);
  assert(warehouse->robot_moves != NULL && "malloc failed");

  for (size_t i = 0; i < moves_txt.num_lines; i++) {
    for (size_t j = 0; j < move_txt_line_length; j++) {
      if (warehouse->num_robot_moves == robot_moves_size) {
        robot_moves_size *= 2;
        warehouse->robot_moves =
            realloc(warehouse->robot_moves,
                    sizeof(*warehouse->robot_moves) * robot_moves_size);
        assert(warehouse->robot_moves != NULL && "realloc failed");
      }

      char ch = moves_txt.lines[i][j];
      RobotMove move;

      switch (ch) {
      case '^':
        move = ROBOT_MOVE_UP;
        break;
      case 'v':
        move = ROBOT_MOVE_DOWN;
        break;
      case '<':
        move = ROBOT_MOVE_LEFT;
        break;
      case '>':
        move = ROBOT_MOVE_RIGHT;
        break;
      default:
        fprintf(stderr, "Invalid robot move [%c]\n", ch);
        exit(EXIT_FAILURE);
      }

      warehouse->robot_moves[warehouse->num_robot_moves++] = move;
    }
  }

  warehouse->robot_moves =
      realloc(warehouse->robot_moves,
              sizeof(*warehouse->robot_moves) * warehouse->num_robot_moves);
  assert(warehouse->robot_moves != NULL && "realloc failed");

  split_txt_free(split_txt);

  return warehouse;
}

char ROBOT_MOVE_CHARS[] = {[ROBOT_MOVE_UP] = '^',
                           [ROBOT_MOVE_DOWN] = 'v',
                           [ROBOT_MOVE_LEFT] = '<',
                           [ROBOT_MOVE_RIGHT] = '>'};

void warehouse_print(Warehouse *warehouse, size_t current_move) {
  for (size_t i = 0; i < warehouse->map_rows; i++) {
    for (size_t j = 0; j < warehouse->map_columns; j++) {
      Tile tile = warehouse->map[i][j];
      if (tile == TILE_WALL) {
        ansi_esc(ANSI_CODE_FAINT);
        ansi_esc(ANSI_CODE_FG_RED);
        putchar('#');
      } else if (tile == TILE_BOX) {
        ansi_esc(ANSI_CODE_FG_GREEN);
        putchar('O');
      } else if (tile == TILE_ROBOT) {
        putchar('@');
      } else {
        ansi_esc(ANSI_CODE_FAINT);
        ansi_esc(ANSI_CODE_FG_YELLOW);
        putchar('.');
      }
      ansi_reset();
    }
    putchar('\n');
  }

  for (size_t i = 0; i < warehouse->num_robot_moves; i++) {
    RobotMove move = warehouse->robot_moves[i];
    if (i < current_move) {
      ansi_esc(ANSI_CODE_FG_BLUE);
    } else if (i == current_move) {
      ansi_esc(ANSI_CODE_BOLD);

    } else {
      ansi_esc(ANSI_CODE_FAINT);
    }
    putchar(ROBOT_MOVE_CHARS[move]);
    ansi_reset();
  }
  putchar('\n');
}

void warehouse_predict(Warehouse *warehouse) {
  for (size_t i = 0; i < warehouse->num_robot_moves; i++) {
    RobotMove move = warehouse->robot_moves[i];
    Tile *robot =
        &warehouse->map[warehouse->robot_pos.row][warehouse->robot_pos.column];

    // TODO: There's probably a neater way of doing this...
    // Up/Down and Left/Right are pretty much the same logic

    if (move == ROBOT_MOVE_UP && warehouse->robot_pos.row > 0) {
      Tile *up =
          &warehouse
               ->map[warehouse->robot_pos.row - 1][warehouse->robot_pos.column];

      if (*up == TILE_EMPTY) {
        *up = TILE_ROBOT;
        *robot = TILE_EMPTY;
        warehouse->robot_pos.row--;
      } else if (*up == TILE_BOX) {
        size_t row = warehouse->robot_pos.row - 1;
        while (*up != TILE_WALL && *up != TILE_EMPTY) {
          row--;
          up = &warehouse->map[row][warehouse->robot_pos.column];
        }

        if (*up == TILE_EMPTY) {
          while (row < warehouse->robot_pos.row) {
            *up = TILE_BOX;
            row++;
            up = &warehouse->map[row][warehouse->robot_pos.column];
          }
          *robot = TILE_EMPTY;
          warehouse->robot_pos.row--;
          warehouse
              ->map[warehouse->robot_pos.row][warehouse->robot_pos.column] =
              TILE_ROBOT;
        }
      }
    } else if (move == ROBOT_MOVE_DOWN &&
               warehouse->robot_pos.row < warehouse->map_rows - 1) {
      Tile *down =
          &warehouse
               ->map[warehouse->robot_pos.row + 1][warehouse->robot_pos.column];

      if (*down == TILE_EMPTY) {
        *down = TILE_ROBOT;
        *robot = TILE_EMPTY;
        warehouse->robot_pos.row++;
      } else if (*down == TILE_BOX) {
        size_t row = warehouse->robot_pos.row + 1;
        while (*down != TILE_WALL && *down != TILE_EMPTY) {
          row++;
          down = &warehouse->map[row][warehouse->robot_pos.column];
        }

        if (*down == TILE_EMPTY) {
          while (row > warehouse->robot_pos.row) {
            *down = TILE_BOX;
            row--;
            down = &warehouse->map[row][warehouse->robot_pos.column];
          }
          *robot = TILE_EMPTY;
          warehouse->robot_pos.row++;
          warehouse
              ->map[warehouse->robot_pos.row][warehouse->robot_pos.column] =
              TILE_ROBOT;
        }
      }
    } else if (move == ROBOT_MOVE_LEFT && warehouse->robot_pos.column > 0) {
      Tile *left =
          &warehouse
               ->map[warehouse->robot_pos.row][warehouse->robot_pos.column - 1];

      if (*left == TILE_EMPTY) {
        *left = TILE_ROBOT;
        *robot = TILE_EMPTY;
        warehouse->robot_pos.column--;
      } else if (*left == TILE_BOX) {
        while (*left != TILE_WALL && *left != TILE_EMPTY) {
          left--;
        }

        if (*left == TILE_EMPTY) {
          while (left < robot) {
            *left = TILE_BOX;
            left++;
          }
          *robot = TILE_EMPTY;
          *(robot - 1) = TILE_ROBOT;
          warehouse->robot_pos.column--;
        }
      }
    } else if (move == ROBOT_MOVE_RIGHT &&
               warehouse->robot_pos.column < warehouse->map_columns - 1) {
      Tile *right =
          &warehouse
               ->map[warehouse->robot_pos.row][warehouse->robot_pos.column + 1];

      if (*right == TILE_EMPTY) {
        *right = TILE_ROBOT;
        *robot = TILE_EMPTY;
        warehouse->robot_pos.column++;
      } else if (*right == TILE_BOX) {
        while (*right != TILE_WALL && *right != TILE_EMPTY) {
          right++;
        }

        if (*right == TILE_EMPTY) {
          while (right > robot) {
            *right = TILE_BOX;
            right--;
          }
          *robot = TILE_EMPTY;
          *(robot + 1) = TILE_ROBOT;
          warehouse->robot_pos.column++;
        }
      }
    }
#ifdef VISUALISE
    ansi_clear();
    warehouse_print(warehouse, i);
    usleep(25 * 1000);
#endif
  }
}

uint64_t warehouse_box_gps_sum(Warehouse *warehouse) {
  uint64_t gps_sum = 0;

  for (size_t i = 0; i < warehouse->map_rows; i++) {
    for (size_t j = 0; j < warehouse->map_columns; j++) {
      Tile tile = warehouse->map[i][j];
      if (tile == TILE_BOX) {
        gps_sum += (100 * i) + (j);
      }
    }
  }

  return gps_sum;
}

int main(void) {
  Txt *txt = txt_read(stdin);
  Warehouse *warehouse = warehouse_parse(txt);

  warehouse_predict(warehouse);
  uint64_t box_gps_sum = warehouse_box_gps_sum(warehouse);

  warehouse_free(warehouse);
  txt_free(txt);

  print_day(15, "Warehouse Woes");
  print_part(1, box_gps_sum, PART1_ANSWER);
  // print_part(2, 0, PART2_ANSWER);

  return 0;
}
