/*
Day 14: Restroom Redoubt
https://adventofcode.com/2024/day/14
*/

#define _DEFAULT_SOURCE
#include <assert.h>
#include <inttypes.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "../lib/ansi.h"
#include "../lib/aoc.h"
#include "../lib/txt.h"

// #define USE_EXAMPLE

#ifdef USE_EXAMPLE
#define TILES_TALL 7
#define TILES_WIDE 11
#define PART1_ANSWER 12
#define PART2_ANSWER 2
#else
#define TILES_TALL 103
#define TILES_WIDE 101
#define PART1_ANSWER 224357412
#define PART2_ANSWER 7083
#endif

#define MIDDLE_ROW (TILES_TALL / 2)
#define MIDDLE_COLUMN (TILES_WIDE / 2)

typedef struct {
  int64_t x, y;
} Vec2;

typedef struct {
  Vec2 initial_position, velocity;
} Robot;

typedef struct {
  Robot *robots;
  size_t length;
} RobotList;

RobotList robots_parse(Txt *txt) {
  RobotList list = {.robots = malloc(sizeof(*list.robots) * txt->num_lines),
                    .length = txt->num_lines};
  assert(list.robots != NULL && "malloc failed");

  for (size_t i = 0; i < list.length; i++) {
    Robot robot;
    int matched = sscanf(txt->lines[i],
                         "p=%" PRId64 ",%" PRId64 " v=%" PRId64 ",%" PRId64,
                         &robot.initial_position.x, &robot.initial_position.y,
                         &robot.velocity.x, &robot.velocity.y);
    assert(matched == 4 && "sscanf failed");

    list.robots[i] = robot;
  }

  return list;
}

int64_t positive_modulo(int64_t value, int64_t mod) {
  // (-1, 103)
  // (-1 % 103 + 103) % 103
  // (-1 + 103) % 103
  // 102 % 103
  // 102
  return (value % mod + mod) % mod;
}

Vec2 robot_simulate(Robot *robot, int64_t seconds) {
  Vec2 pos = robot->initial_position;

  pos.y = (pos.y + (robot->velocity.y * seconds)) % TILES_TALL;
  if (pos.y < 0) {
    pos.y = positive_modulo(pos.y, TILES_TALL);
  }

  pos.x = (pos.x + (robot->velocity.x * seconds)) % TILES_WIDE;
  if (pos.x < 0) {
    pos.x = positive_modulo(pos.x, TILES_WIDE);
  }

  return pos;
}

uint64_t robot_count(uint64_t tiles[TILES_TALL][TILES_WIDE], Vec2 start,
                     Vec2 end) {
  uint64_t count = 0;

  for (int64_t row = start.y; row <= end.y; row++) {
    for (int64_t col = start.x; col <= end.x; col++) {
      count += tiles[row][col];
    }
  }

  return count;
}

uint64_t safety_factor(uint64_t tiles[TILES_TALL][TILES_WIDE]) {
  const int64_t middle_row = TILES_TALL / 2;
  const int64_t middle_col = TILES_WIDE / 2;

  const Vec2 q1_start = {.x = 0, .y = 0};
  const Vec2 q1_end = {.x = middle_col - 1, .y = middle_row - 1};
  uint64_t q1_count = robot_count(tiles, q1_start, q1_end);

  const Vec2 q2_start = {.x = middle_col + 1, .y = 0};
  const Vec2 q2_end = {.x = TILES_WIDE - 1, .y = middle_row - 1};
  uint64_t q2_count = robot_count(tiles, q2_start, q2_end);

  const Vec2 q3_start = {.x = 0, .y = middle_row + 1};
  const Vec2 q3_end = {.x = middle_col - 1, .y = TILES_TALL - 1};
  uint64_t q3_count = robot_count(tiles, q3_start, q3_end);

  const Vec2 q4_start = {.x = middle_col + 1, .y = middle_row + 1};
  const Vec2 q4_end = {.x = TILES_WIDE - 1, .y = TILES_TALL - 1};
  uint64_t q4_count = robot_count(tiles, q4_start, q4_end);

  return q1_count * q2_count * q3_count * q4_count;
}

void print_easter_egg_tiles(uint64_t tiles[TILES_TALL][TILES_WIDE]) {
  const size_t tree_row_start = 38;
  const size_t tree_rows = 33;
  const size_t tree_col_start = 23;
  const size_t tree_cols = 31;

  putchar('\n');
  for (size_t row = tree_row_start; row < tree_row_start + tree_rows; row++) {
    for (size_t col = tree_col_start; col < tree_col_start + tree_cols; col++) {
      uint64_t num_robots = tiles[row][col];
      if (num_robots == 0) {
        ansi_esc(ANSI_CODE_FAINT);
        ansi_esc(ANSI_CODE_FG_RED);
        putchar('.');
      } else {
        ansi_esc(ANSI_CODE_FG_GREEN);
        putchar('#');
      }
    }
    putchar('\n');
  }
}

int main(void) {
  Txt *txt = txt_read(stdin);
  RobotList robot_list = robots_parse(txt);
  uint64_t tiles[TILES_TALL][TILES_WIDE] = {0};

  for (size_t i = 0; i < robot_list.length; i++) {
    Vec2 robot_pos = robot_simulate(&robot_list.robots[i], 100);
    tiles[robot_pos.y][robot_pos.x]++;
  }

  uint64_t safety = safety_factor(tiles);

  for (size_t i = 0; i < TILES_TALL; i++) {
    for (size_t j = 0; j < TILES_WIDE; j++) {
      tiles[i][j] = 0;
    }
  }

  // Can use the safety factor as a heuristic to find the easter egg.
  // As the robots get closer to the centre to form the easter egg image
  // (a Christmas tree) the safety factor will become smaller.

  // It only took 4 reductions in the safety factor to find the easter egg for
  // my puzzle input.

  for (size_t i = 0; i < robot_list.length; i++) {
    Vec2 robot_pos = robot_simulate(&robot_list.robots[i], PART2_ANSWER);
    tiles[robot_pos.y][robot_pos.x]++;
  }

  free(robot_list.robots);
  txt_free(txt);

  print_day(14, "Restroom Redoubt");
  print_part(1, safety, PART1_ANSWER);
  print_part(2, PART2_ANSWER, PART2_ANSWER);
  print_easter_egg_tiles(tiles);

  return 0;
}
