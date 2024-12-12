/*
Day 12: Garden Groups
https://adventofcode.com/2024/day/12
*/

// #define USE_EXAMPLE
#define _DEFAULT_SOURCE

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "../lib/aoc.h"
#include "../lib/txt.h"
#include "main.h"

typedef struct {
  size_t row, column;
} Coord;

typedef struct {
  char plant;
  Coord location;
} GardenPlot;

typedef struct {
  char **plots;
  size_t rows, columns;
} GardenMap;

uint64_t plot_region_fencing_cost(GardenMap *map, GardenPlot *plot,
                                  bool visited[]) {
  uint64_t region_perimiter = 0;
  uint64_t region_area = 0;

  Coord stack[map->rows * map->columns];
  size_t stack_pointer = 1;
  stack[stack_pointer] = plot->location;

  while (stack_pointer > 0) {
    Coord popped = stack[stack_pointer--];

    size_t popped_visited_index = (popped.row * map->columns) + popped.column;
    if (visited[popped_visited_index]) {
      continue;
    }

    visited[popped_visited_index] = true;
    region_area++;

    uint8_t num_touching = 0;
    Coord up = {.row = popped.row - 1, .column = popped.column};
    Coord down = {.row = popped.row + 1, .column = popped.column};
    Coord left = {.row = popped.row, .column = popped.column - 1};
    Coord right = {.row = popped.row, .column = popped.column + 1};

    if (popped.row < map->rows) {
      if (popped.row > 0 && map->plots[up.row][up.column] == plot->plant) {
        num_touching++;
        stack[++stack_pointer] = up;
      }

      if ((popped.row < map->rows - 1) &&
          map->plots[down.row][down.column] == plot->plant) {
        num_touching++;
        stack[++stack_pointer] = down;
      }
    }

    if (popped.column < map->columns) {
      if (popped.column > 0 &&
          map->plots[left.row][left.column] == plot->plant) {
        num_touching++;
        stack[++stack_pointer] = left;
      }

      if ((popped.column < map->columns - 1) &&
          map->plots[right.row][right.column] == plot->plant) {
        num_touching++;
        stack[++stack_pointer] = right;
      }
    }

    region_perimiter += 4 - num_touching;
  }

  return region_perimiter * region_area;
}

uint64_t garden_fencing_cost(Txt *garden_map) {
  size_t num_columns = strlen(garden_map->lines[0]);
  size_t num_plots = garden_map->num_lines * num_columns;

  bool visited[num_plots];
  for (size_t plot = 0; plot < num_plots; plot++) {
    visited[plot] = false;
  }

  uint64_t cost = 0;

  for (size_t row = 0; row < garden_map->num_lines; row++) {
    for (size_t col = 0; col < num_columns; col++) {
      if (!visited[(row * num_columns) + col]) {
        cost += plot_region_fencing_cost(
            &(GardenMap){.plots = garden_map->lines,
                         .rows = garden_map->num_lines,
                         .columns = num_columns},
            &(GardenPlot){.plant = garden_map->lines[row][col],
                          .location = (Coord){.row = row, .column = col}},
            visited);
      }
    }
  }

  for (size_t plot = 0; plot < num_plots; plot++) {
    assert(visited[plot] && "plot not visited");
  }

  return cost;
}

int main(void) {
  Txt *garden_map = txt_read(stdin);

  uint64_t fencing_cost = garden_fencing_cost(garden_map);

  txt_free(garden_map);

  print_day(12, "Garden Groups");
  print_part(1, fencing_cost, PART1_ANSWER);
  // print_part(2, 2, PART2_ANSWER);

  return 0;
}
