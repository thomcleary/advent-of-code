/*
Day 15: Warehouse Woes
https://adventofcode.com/2024/day/15
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
#include <unistd.h>

#include "../lib/ansi.h"
#include "../lib/aoc.h"
#include "../lib/txt.h"

// #define USE_EXAMPLE

#ifdef USE_EXAMPLE
#define PART1_ANSWER 10092
#define PART2_ANSWER 9021
#else
#define PART1_ANSWER 1465523
#define PART2_ANSWER 1471049
#endif

#define ANIMATION_SPEED_MS 16

#define UP '^'
#define DOWN 'v'
#define LEFT '<'
#define RIGHT '>'

#define WALL '#'
#define BOX 'O'
#define WIDE_BOX_LEFT '['
#define WIDE_BOX_RIGHT ']'
#define ROBOT '@'
#define EMPTY '.'

typedef struct {
  int64_t x, y;
} Vec2;

typedef struct {
  size_t row, column;
} Coord;

typedef struct {
  bool wide;

  char **map;
  size_t map_rows, map_columns;
  Vec2 robot_pos;

  char *moves;
  size_t num_moves;
  size_t current_move;
} Warehouse;

void warehouse_free(Warehouse *warehouse) {
  for (size_t i = 0; i < warehouse->map_rows; i++) {
    free(warehouse->map[i]);
  }
  free(warehouse->map);
  free(warehouse->moves);
  free(warehouse);
}

Warehouse *warehouse_parse(Txt *txt) {
  SplitTxt *split_txt = txt_split(txt, "");
  assert(split_txt->num_txts == 2 && "invalid puzzle input");
  Txt map_txt = split_txt->txts[0];
  Txt moves_txt = split_txt->txts[1];

  Warehouse *warehouse = malloc(sizeof(*warehouse));
  assert(warehouse != NULL && "malloc failed");

  warehouse->wide = false;
  warehouse->map_rows = map_txt.num_lines;
  warehouse->map_columns = strlen(map_txt.lines[0]);
  warehouse->map = malloc(sizeof(*warehouse->map) * warehouse->map_rows);
  assert(warehouse->map != NULL && "malloc failed");

  for (size_t i = 0; i < warehouse->map_rows; i++) {
    char *line = map_txt.lines[i];
    warehouse->map[i] =
        malloc(sizeof(*warehouse->map[i]) * warehouse->map_columns);
    assert(warehouse->map[i] != NULL && "malloc failed");
    for (size_t j = 0; j < warehouse->map_columns; j++) {
      char tile = line[j];
      warehouse->map[i][j] = tile;

      if (tile == ROBOT) {
        warehouse->robot_pos = (Vec2){.x = (int64_t)i, .y = (int64_t)j};
      }
    }
  }

  size_t move_txt_line_length = strlen(moves_txt.lines[0]);
  size_t robot_moves_size = moves_txt.num_lines * move_txt_line_length;

  warehouse->num_moves = 0;
  warehouse->current_move = 0;
  warehouse->moves = malloc(sizeof(*warehouse->moves) * robot_moves_size);
  assert(warehouse->moves != NULL && "malloc failed");

  for (size_t i = 0; i < moves_txt.num_lines; i++) {
    for (size_t j = 0; j < move_txt_line_length; j++) {
      if (warehouse->num_moves == robot_moves_size) {
        robot_moves_size *= 2;
        warehouse->moves = realloc(warehouse->moves, sizeof(*warehouse->moves) *
                                                         robot_moves_size);
        assert(warehouse->moves != NULL && "realloc failed");
      }

      warehouse->moves[warehouse->num_moves++] = moves_txt.lines[i][j];
    }
  }

  warehouse->moves = realloc(warehouse->moves,
                             sizeof(*warehouse->moves) * warehouse->num_moves);
  assert(warehouse->moves != NULL && "realloc failed");

  split_txt_free(split_txt);

  return warehouse;
}

void warehouse_print(Warehouse *warehouse, size_t current_move) {
  size_t map_str_length =
      (warehouse->map_rows * warehouse->map_columns) + warehouse->map_rows;

  char map_buf[map_str_length + 1];
  size_t map_buf_index = 0;
  for (size_t i = 0; i < warehouse->map_rows; i++) {
    for (size_t j = 0; j < warehouse->map_columns; j++) {
      map_buf[map_buf_index++] = warehouse->map[i][j];
    }
    map_buf[map_buf_index++] = '\n';
  }
  map_buf[map_buf_index] = '\0';

  size_t max_show = warehouse->map_columns - strlen("Move: ^  ");
  size_t moves_remaining = warehouse->num_moves - current_move + 1;
  size_t show_moves = moves_remaining < max_show ? moves_remaining : max_show;

  char move_buf[show_moves + 1];
  size_t move_buf_index = 0;
  for (size_t i = 0; i < show_moves; i++) {
    move_buf[move_buf_index++] = warehouse->moves[current_move + 1 + i];
  }
  move_buf[move_buf_index] = '\0';

  printf("%s\n", map_buf);
  printf("Move: %c  ", warehouse->moves[current_move]);
  printf("%s\n", move_buf);
  printf("(%zu/%zu)\n\n", current_move + 1, warehouse->num_moves);
}

bool push(Warehouse *warehouse, Vec2 tile_pos, Vec2 direction, bool check) {
  char *tile = &warehouse->map[tile_pos.x][tile_pos.y];

  Vec2 adjacent_vec = {.x = tile_pos.x + direction.x,
                       .y = tile_pos.y + direction.y};
  char *adjacent = &warehouse->map[adjacent_vec.x][adjacent_vec.y];

  if (*adjacent == WALL) {
    return false;
  }

  bool adjacent_is_wide_box =
      *adjacent == WIDE_BOX_LEFT || *adjacent == WIDE_BOX_RIGHT;

  if (*adjacent == BOX || (adjacent_is_wide_box && direction.x == 0)) {
    if (!push(warehouse, adjacent_vec, direction, check)) {
      return false;
    }
  } else if (adjacent_is_wide_box) {
    Vec2 other_half = {.x = adjacent_vec.x,
                       .y = adjacent_vec.y +
                            (*adjacent == WIDE_BOX_LEFT ? 1 : -1)};

    bool blocked = !(push(warehouse, adjacent_vec, direction, true) &&
                     push(warehouse, other_half, direction, true));

    if (blocked) {
      return false;
    }

    if (!check) {
      push(warehouse, adjacent_vec, direction, false);
      push(warehouse, other_half, direction, false);
      warehouse->map[other_half.x][other_half.y] = EMPTY;
    }
  }

  if (!check) {
    *adjacent = *tile;
    *tile = EMPTY;
  }

  return true;
}

void warehouse_predict(Warehouse *warehouse, bool visualise) {
  const Vec2 up_vec = {.x = -1, .y = 0};
  const Vec2 down_vec = {.x = 1, .y = 0};
  const Vec2 left_vec = {.x = 0, .y = -1};
  const Vec2 right_vec = {.x = 0, .y = 1};

  for (size_t i = 0; i < warehouse->num_moves; i++) {
    Vec2 direction;
    switch (warehouse->moves[i]) {
    case UP:
      direction = up_vec;
      break;
    case DOWN:
      direction = down_vec;
      break;
    case LEFT:
      direction = left_vec;
      break;
    default:
      direction = right_vec;
      break;
    }

    if (push(warehouse, warehouse->robot_pos, direction, false)) {
      warehouse->robot_pos.x += direction.x;
      warehouse->robot_pos.y += direction.y;
    };

    if (visualise) {
      ansi_clear();
      warehouse_print(warehouse, i);
      usleep(ANIMATION_SPEED_MS * 1000);
    }
  }
}

uint64_t warehouse_box_gps_sum(Warehouse *warehouse) {
  uint64_t gps_sum = 0;

  for (size_t i = 0; i < warehouse->map_rows; i++) {
    for (size_t j = 0; j < warehouse->map_columns; j++) {
      char tile = warehouse->map[i][j];
      if (tile == BOX || tile == WIDE_BOX_LEFT) {
        gps_sum += (100 * i) + (j);
      }
    }
  }

  return gps_sum;
}

Warehouse *warehouse_widen(Warehouse *warehouse) {
  if (warehouse->wide) {
    return warehouse;
  }

  char buf[warehouse->map_rows][warehouse->map_columns * 2];

  for (size_t i = 0; i < warehouse->map_rows; i++) {
    for (size_t j = 0; j < warehouse->map_columns; j++) {
      char tile = warehouse->map[i][j];
      char left, right;
      if (tile == WALL) {
        left = tile;
        right = tile;
      } else if (tile == BOX) {
        left = WIDE_BOX_LEFT;
        right = WIDE_BOX_RIGHT;
      } else if (tile == ROBOT) {
        left = ROBOT;
        right = EMPTY;
        warehouse->robot_pos = (Vec2){.x = (int64_t)i, .y = (int64_t)j * 2};
      } else {
        left = EMPTY;
        right = EMPTY;
      }

      buf[i][j * 2] = left;
      buf[i][j * 2 + 1] = right;
    }
  }

  warehouse->wide = true;
  warehouse->map_columns *= 2;

  for (size_t i = 0; i < warehouse->map_rows; i++) {
    warehouse->map[i] = realloc(warehouse->map[i], sizeof(*warehouse->map[i]) *
                                                       warehouse->map_columns);
    assert(warehouse->map[i] != NULL && "realloc failed");

    for (size_t j = 0; j < warehouse->map_columns; j++) {
      warehouse->map[i][j] = buf[i][j];
    }
  }

  return warehouse;
}

int main(void) {
  Txt *txt = txt_read(stdin);

  bool visualise = false;
  Warehouse *warehouse = warehouse_parse(txt);
  warehouse_predict(warehouse, visualise);
  uint64_t box_gps_sum = warehouse_box_gps_sum(warehouse);

  visualise = false;
  Warehouse *wide_warehouse = warehouse_widen(warehouse_parse(txt));
  warehouse_predict(wide_warehouse, visualise);
  uint64_t wide_box_gps_sum = warehouse_box_gps_sum(wide_warehouse);

  warehouse_free(warehouse);
  warehouse_free(wide_warehouse);
  txt_free(txt);

  print_day(15, "Warehouse Woes");
  print_part_uint64(1, box_gps_sum, PART1_ANSWER);
  print_part_uint64(2, wide_box_gps_sum, PART2_ANSWER);

  return 0;
}
