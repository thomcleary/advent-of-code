/*
Day 16: Reindeer Maze
https://adventofcode.com/2024/day/16
*/

#define _DEFAULT_SOURCE
#include <assert.h>
#include <inttypes.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../lib/aoc.h"
#include "../lib/binary-heap.h"
#include "../lib/txt.h"

// #define USE_EXAMPLE

#ifdef USE_EXAMPLE
#define INPUT_FILENAME "example-input.txt"
#define PART1_ANSWER 11048
#define PART2_ANSWER 64
#else
#define INPUT_FILENAME "puzzle-input.txt"
#define PART1_ANSWER 88416
#define PART2_ANSWER 442
#endif

#define NUM_DIRECTIONS 4

typedef enum { NORTH = 0, SOUTH = 1, EAST = 2, WEST = 3 } Direction;

typedef struct {
  int64_t x, y;
} Vec2;

typedef struct MazeNode {
  bool seen;
  int64_t score;
  Vec2 position;
  Direction direction;
  struct MazeNode *from[NUM_DIRECTIONS];
  size_t num_from;
} MazeNode;

typedef struct {
  char **tiles;
  size_t rows, columns;
  Vec2 start, end;
} Maze;

typedef struct {
  int64_t lowest_score;
  uint64_t num_best_seats;
} MazeDetails;

static const struct {
  char WALL, EMPTY, START, END;
} MAZE_TILE = {.WALL = '#', .EMPTY = '.', .START = 'S', .END = 'E'};

void maze_free(Maze *maze) {
  for (size_t r = 0; r < maze->rows; r++) {
    free(maze->tiles[r]);
  }
  free(maze->tiles);
  free(maze);
}

Maze *maze_parse(Txt *txt) {
  Maze *maze = malloc(sizeof(*maze));
  assert(maze != NULL && "malloc failed");

  assert(txt->num_lines > 0 && "invalid puzzle input");
  maze->rows = txt->num_lines;
  maze->columns = strlen(txt->lines[0]);

  maze->tiles = malloc(sizeof(*maze->tiles) * maze->rows);
  assert(maze->tiles != NULL && "malloc failed");

  for (size_t r = 0; r < maze->rows; r++) {
    maze->tiles[r] = malloc(sizeof(*maze->tiles[r]) * maze->columns);
    assert(maze->tiles[r] != NULL && "malloc failed");

    for (size_t c = 0; c < maze->columns; c++) {
      char tile = txt->lines[r][c];
      maze->tiles[r][c] = tile;

      if (tile == MAZE_TILE.START) {
        maze->start = (Vec2){.x = (int64_t)r, .y = (int64_t)c};
      } else if (tile == MAZE_TILE.END) {
        maze->end = (Vec2){.x = (int64_t)r, .y = (int64_t)c};
      }
    }
  }

  return maze;
}

int64_t compare_maze_nodes(const void *a, const void *b) {
  int64_t a_score = (*(MazeNode *)a).score;
  int64_t b_score = (*(MazeNode *)b).score;

  // Lower score is higher priority
  if (a_score < b_score) {
    return 1;
  }

  if (a_score > b_score) {
    return -1;
  }

  return 0;
}

void maze_nodes_reset(size_t rows, size_t cols,
                      MazeNode nodes[rows][cols][NUM_DIRECTIONS]) {
  for (size_t r = 0; r < rows; r++) {
    for (size_t c = 0; c < cols; c++) {
      MazeNode unseen = {.seen = false,
                         .position = {.x = (int64_t)r, .y = (int64_t)c},
                         .num_from = 0};

      unseen.direction = NORTH;
      nodes[r][c][NORTH] = unseen;

      unseen.direction = SOUTH;
      nodes[r][c][SOUTH] = unseen;

      unseen.direction = EAST;
      nodes[r][c][EAST] = unseen;

      unseen.direction = WEST;
      nodes[r][c][WEST] = unseen;
    }
  }
}

bool is_same_position(Vec2 a, Vec2 b) {
  return a.x == b.x && a.y == b.y;
}

Vec2 step_north(Vec2 vec) {
  vec.x--;
  return vec;
}

Vec2 step_south(Vec2 vec) {
  vec.x++;
  return vec;
}

Vec2 step_east(Vec2 vec) {
  vec.y++;
  return vec;
}

Vec2 step_west(Vec2 vec) {
  vec.y--;
  return vec;
}

bool is_wall_tile(Maze *maze, Vec2 tile) {
  return maze->tiles[tile.x][tile.y] == MAZE_TILE.WALL;
}

uint64_t num_best_seats(Maze *maze, MazeNode *end_nodes[NUM_DIRECTIONS],
                        size_t num_end_nodes) {
  bool best_seats[maze->rows][maze->columns];
  memset(best_seats, false, sizeof(best_seats));
  uint64_t num_best_seats = 0;

  MazeNode *backtrack_nodes[maze->rows * maze->columns * NUM_DIRECTIONS];
  size_t backtrack_nodes_length = 0;

  for (size_t i = 0; i < num_end_nodes; i++) {
    backtrack_nodes[backtrack_nodes_length++] = end_nodes[i];
  }

  while (backtrack_nodes_length > 0) {
    MazeNode *backtrack_node = backtrack_nodes[backtrack_nodes_length - 1];
    backtrack_nodes_length--;

    bool *seat =
        &best_seats[backtrack_node->position.x][backtrack_node->position.y];

    if (!(*seat)) {
      *seat = true;
      num_best_seats++;
    }

    for (size_t i = 0; i < backtrack_node->num_from; i++) {
      backtrack_nodes[backtrack_nodes_length++] = backtrack_node->from[i];
    }
  }

  return num_best_seats;
}

MazeDetails maze_search(Maze *maze) {
  BinaryHeap *priorityq = binaryheap_new(compare_maze_nodes);

  MazeNode nodes[maze->rows][maze->columns][NUM_DIRECTIONS];
  maze_nodes_reset(maze->rows, maze->columns, nodes);

  MazeNode *start = &nodes[maze->start.x][maze->start.y][EAST];
  *start = (MazeNode){.seen = true,
                      .position = maze->start,
                      .direction = EAST,
                      .num_from = 0,
                      .score = 0};

  MazeNode *end_nodes[NUM_DIRECTIONS];
  size_t num_end_nodes = 0;
  int64_t lowest_score = INT64_MAX;

  binaryheap_push(priorityq, start);

  while (!binaryheap_is_empty(priorityq)) {
    Option node_option = binaryheap_pop(priorityq);
    assert(node_option.some);
    MazeNode *curr_node = node_option.value;

    if (is_same_position(curr_node->position, maze->end)) {
      if (curr_node->score <= lowest_score) {
        lowest_score = curr_node->score;
        end_nodes[num_end_nodes++] = curr_node;
      }
      continue;
    }

    struct NextStep {
      Direction direction;
      Vec2 to;
    } next[NUM_DIRECTIONS] = {
        {.direction = NORTH, .to = step_north(curr_node->position)},
        {.direction = SOUTH, .to = step_south(curr_node->position)},
        {.direction = EAST, .to = step_east(curr_node->position)},
        {.direction = WEST, .to = step_west(curr_node->position)},
    };

    for (size_t i = 0; i < NUM_DIRECTIONS; i++) {
      struct NextStep next_step = next[i];

      MazeNode *next_node =
          &nodes[next_step.to.x][next_step.to.y][next_step.direction];

      int64_t next_step_score =
          (curr_node->score) +
          (next_step.direction != curr_node->direction ? 1001 : 1);

      if (!is_wall_tile(maze, next_step.to) &&
          (!next_node->seen || next_step_score <= next_node->score)) {
        next_node->score = next_step_score;
        next_node->from[next_node->num_from++] = curr_node;

        if (!next_node->seen) {
          next_node->seen = true;
          binaryheap_push(priorityq, next_node);
        }
      }
    }
  }
  assert(num_end_nodes > 0 && "end of maze not found");

  binaryheap_free(priorityq);

  return (MazeDetails){.lowest_score = lowest_score,
                       .num_best_seats =
                           num_best_seats(maze, end_nodes, num_end_nodes)};
}

int main(void) {
  Txt *txt = txt_read_file(INPUT_FILENAME);

  Maze *maze = maze_parse(txt);
  MazeDetails maze_details = maze_search(maze);

  maze_free(maze);
  txt_free(txt);

  print_day(16, "Reindeer Maze");
  print_part(1, (uint64_t)maze_details.lowest_score, PART1_ANSWER);
  print_part(2, maze_details.num_best_seats, PART2_ANSWER);

  return 0;
}
