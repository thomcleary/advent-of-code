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

#include "../lib/ansi.h"
#include "../lib/aoc.h"
#include "../lib/binary-heap.h"
#include "../lib/txt.h"

// #define USE_EXAMPLE

#ifdef USE_EXAMPLE
#define INPUT_FILENAME "example-input.txt"
#define PART1_ANSWER 11048
#define PART2_ANSWER 2
#else
#define INPUT_FILENAME "puzzle-input.txt"
#define PART1_ANSWER 88416
#define PART2_ANSWER 2
#endif

#define NUM_DIRECTIONS 4

typedef enum { NORTH = 0, SOUTH = 1, EAST = 2, WEST = 3 } Direction;

typedef struct {
  int64_t x, y;
} Vec2;

typedef struct {
  Vec2 position;
  Vec2 from;
  Direction direction;
  int64_t cost;
} MazeNode;

typedef struct {
  Vec2 position;
  Direction direction;
} Reindeer;

typedef struct {
  char **tiles;
  size_t rows, columns;
  Vec2 start, end;
} Maze;

static const struct {
  char WALL, EMPTY, START, END;
} MAZE_TILE = {.WALL = '#', .EMPTY = '.', .START = 'S', .END = 'E'};

static const char DIRECTION_TILE[] = {
    [NORTH] = '^', [SOUTH] = 'v', [EAST] = '>', [WEST] = '<'};

bool vec2_equal(Vec2 a, Vec2 b) {
  return a.x == b.x && a.y == b.y;
}

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

void maze_print(Maze *maze, Reindeer reindeer) {
  for (size_t r = 0; r < maze->rows; r++) {
    for (size_t c = 0; c < maze->columns; c++) {
      char tile = maze->tiles[r][c];
      if (reindeer.position.x == (int64_t)r &&
          reindeer.position.y == (int64_t)c) {
        tile = DIRECTION_TILE[reindeer.direction];
      }

      if (tile == MAZE_TILE.WALL) {
        ansi_esc(ANSI_CODE_FG_GREEN);
      } else if (tile == MAZE_TILE.EMPTY) {
        ansi_esc(ANSI_CODE_FG_YELLOW);
        ansi_esc(ANSI_CODE_FAINT);
      } else if (tile == MAZE_TILE.START) {
        ansi_esc(ANSI_CODE_FG_CYAN);
      } else if (tile == MAZE_TILE.END) {
        ansi_esc(ANSI_CODE_BOLD);
        ansi_esc(ANSI_CODE_FG_YELLOW);
      } else {
        ansi_esc(ANSI_CODE_BOLD);
        ansi_esc(ANSI_CODE_FG_RED);
      }

      putchar(tile);
      ansi_reset();
    }
    putchar('\n');
  }
  putchar('\n');
}

int64_t compare_maze_nodes(const void *a, const void *b) {
  int64_t a_cost = (*(MazeNode *)a).cost;
  int64_t b_cost = (*(MazeNode *)b).cost;

  // Lower cost is higher priority
  if (a_cost < b_cost) {
    return 1;
  }

  if (a_cost > b_cost) {
    return -1;
  }

  return 0;
}

Vec2 step_north(Vec2 vec) {
  vec.x--;
  return vec;
}

Vec2 step_south(Vec2 vec) {
  vec.x++;
  return vec;
}

Vec2 step_west(Vec2 vec) {
  vec.y--;
  return vec;
}

Vec2 step_east(Vec2 vec) {
  vec.y++;
  return vec;
}

bool is_wall_tile(Maze *maze, Vec2 tile) {
  return maze->tiles[tile.x][tile.y] == MAZE_TILE.WALL;
}

int64_t maze_search(Maze *maze) {
  BinaryHeap *priorityq = binaryheap_new(compare_maze_nodes);

  size_t nodes_size = maze->rows * maze->columns * NUM_DIRECTIONS;
  size_t nodes_length = 0;
  MazeNode nodes[nodes_size];
  assert(nodes != NULL && "malloc failed");

  bool seen[maze->rows][maze->columns][NUM_DIRECTIONS];
  memset(seen, false, sizeof(seen));

  nodes[nodes_length++] = (MazeNode){.position = maze->start,
                                     .from = maze->start,
                                     .direction = EAST,
                                     .cost = 0};

  MazeNode *start = &nodes[nodes_length - 1];
  binaryheap_push(priorityq, start);
  seen[start->position.x][start->position.y][start->direction] = true;

  while (!binaryheap_is_empty(priorityq)) {
    Option node_option = binaryheap_pop(priorityq);
    assert(node_option.some);
    MazeNode *node = node_option.value;

    if (vec2_equal(node->position, maze->end)) {
      binaryheap_free(priorityq);
      return node->cost;
    }

    Vec2 north_node = step_north(node->position);
    Vec2 south_node = step_south(node->position);
    Vec2 west_node = step_west(node->position);
    Vec2 east_node = step_east(node->position);

    // TODO: can probably condense this logic
    // Lets wait for part 2 though...

    if (!vec2_equal(node->from, north_node) &&
        !is_wall_tile(maze, north_node) &&
        !seen[north_node.x][north_node.y][NORTH]) {
      MazeNode next_node = {.position = north_node,
                            .from = node->position,
                            .direction = NORTH,
                            .cost = node->cost + 1};

      if (node->direction != NORTH) {
        next_node.cost += 1000;
      }
      nodes[nodes_length++] = next_node;
      binaryheap_push(priorityq, &nodes[nodes_length - 1]);
      seen[north_node.x][north_node.y][NORTH] = true;
    }

    if (!vec2_equal(node->from, south_node) &&
        !is_wall_tile(maze, south_node) &&
        !seen[south_node.x][south_node.y][SOUTH]) {
      MazeNode next_node = {.position = south_node,
                            .from = node->position,
                            .direction = SOUTH,
                            .cost = node->cost + 1};

      if (node->direction != SOUTH) {
        next_node.cost += 1000;
      }

      nodes[nodes_length++] = next_node;
      binaryheap_push(priorityq, &nodes[nodes_length - 1]);
      seen[south_node.x][south_node.y][SOUTH] = true;
    }

    if (!vec2_equal(node->from, west_node) && !is_wall_tile(maze, west_node) &&
        !seen[west_node.x][west_node.y][WEST]) {
      MazeNode next_node = {.position = west_node,
                            .from = node->position,
                            .direction = WEST,
                            .cost = node->cost + 1};

      if (node->direction != WEST) {
        next_node.cost += 1000;
      }

      nodes[nodes_length++] = next_node;
      binaryheap_push(priorityq, &nodes[nodes_length - 1]);
      seen[west_node.x][west_node.y][WEST] = true;
    }

    if (!vec2_equal(node->from, east_node) && !is_wall_tile(maze, east_node) &&
        !seen[east_node.x][east_node.y][EAST]) {
      MazeNode next_node = {.position = east_node,
                            .from = node->position,
                            .direction = EAST,
                            .cost = node->cost + 1};

      if (node->direction != EAST) {
        next_node.cost += 1000;
      }

      nodes[nodes_length++] = next_node;
      binaryheap_push(priorityq, &nodes[nodes_length - 1]);
      seen[east_node.x][east_node.y][EAST] = true;
    }
  }

  binaryheap_free(priorityq);

  return -1;
}

int main(void) {
  Txt *txt = txt_read_file(INPUT_FILENAME);

  Maze *maze = maze_parse(txt);
  int64_t lowest_score = maze_search(maze);

  maze_free(maze);
  txt_free(txt);

  print_day(16, "Reindeer Maze");
  print_part(1, (uint64_t)lowest_score, PART1_ANSWER);
  // print_part(2, 0, PART2_ANSWER);

  return 0;
}
