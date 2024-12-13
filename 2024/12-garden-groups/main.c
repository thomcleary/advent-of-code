/*
Day 12: Garden Groups
https://adventofcode.com/2024/day/12
*/

// #define USE_EXAMPLE
#define _DEFAULT_SOURCE

#include <assert.h>
#include <inttypes.h>
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
  uint64_t area, perimiter, sides;
} Region;

typedef struct {
  char **plots;
  size_t rows, columns;
} GardenMap;

typedef struct {
  uint64_t cost, discount;
} FencingQuote;

Coord coord_up(const Coord coord) {
  return (Coord){.row = coord.row - 1, .column = coord.column};
}

Coord coord_down(const Coord coord) {
  return (Coord){.row = coord.row + 1, .column = coord.column};
}

Coord coord_left(const Coord coord) {
  return (Coord){.row = coord.row, .column = coord.column - 1};
}

Coord coord_right(const Coord coord) {
  return (Coord){.row = coord.row, .column = coord.column + 1};
}

bool is_different_plant(const GardenMap *map, char plant, Coord neighbour) {
  return map->plots[neighbour.row][neighbour.column] != plant;
}

bool is_plot_on_top_edge(Coord plot) {
  return plot.row == 0;
}

bool is_plot_on_bottom_edge(Coord plot, const GardenMap *map) {
  return plot.row == map->rows - 1;
}

bool is_plot_on_left_edge(Coord plot) {
  return plot.column == 0;
}

bool is_plot_on_right_edge(Coord plot, const GardenMap *map) {
  return plot.column == map->columns - 1;
}

uint8_t count_corners(const GardenMap *map, const GardenPlot *plot) {
  const char plant = plot->plant;
  const Coord location = plot->location;

  const Coord up = coord_up(location);
  const Coord down = coord_down(location);
  const Coord left = coord_left(location);
  const Coord right = coord_right(location);

  bool up_edge =
      is_plot_on_top_edge(location) || is_different_plant(map, plant, up);
  bool down_edge = is_plot_on_bottom_edge(location, map) ||
                   is_different_plant(map, plant, down);
  bool left_edge =
      is_plot_on_left_edge(location) || is_different_plant(map, plant, left);
  bool right_edge = is_plot_on_right_edge(location, map) ||
                    is_different_plant(map, plant, right);

  // Corners (C)
  // on the outside border (#)
  // of the region (c)
  // (concave corners, interior angle < 180 degrees)

  // ###
  // #Cc
  // #cc
  int count = up_edge && left_edge;

  // ###
  // cC#
  // cc#
  count += up_edge && right_edge;

  // #cc
  // #Cc
  // ###
  count += down_edge && left_edge;

  // cc#
  // cC#
  // ###
  count += down_edge && right_edge;

  // Corners (C)
  // on the border of regions (X)
  // that are nested within the region (c)
  // (convex corners, interior angle > 180 degrees)

  // XXcc#
  // ccCc#
  // cccc#
  // #####
  count +=
      !up_edge && !left_edge && is_different_plant(map, plant, coord_left(up));

  // #ccXX
  // #cCcc
  // #cccc
  // #####
  count += !up_edge && !right_edge &&
           is_different_plant(map, plant, coord_right(up));

  // #####
  // cccc#
  // ccCc#
  // XXcc#
  count += !down_edge && !left_edge &&
           is_different_plant(map, plant, coord_left(down));

  // #####
  // cccc#
  // cCcc#
  // ccXX#
  count += !down_edge && !right_edge &&
           is_different_plant(map, plant, coord_right(down));

  return (uint8_t)(count);
}

Region find_region(const GardenMap *map, const GardenPlot *plot,
                   bool visited[]) {
  uint64_t area = 0;
  uint64_t perimiter = 0;
  uint64_t sides = 0;

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

    area++;
    sides += count_corners(
        map, &(GardenPlot){.plant = plot->plant, .location = popped});

    const Coord up = coord_up(popped);
    const Coord down = coord_down(popped);
    const Coord left = coord_left(popped);
    const Coord right = coord_right(popped);

    uint8_t num_touching = 0;

    if (!is_plot_on_top_edge(popped) &&
        !is_different_plant(map, plot->plant, up)) {
      num_touching++;
      stack[++stack_pointer] = up;
    }

    if (!is_plot_on_bottom_edge(popped, map) &&
        !is_different_plant(map, plot->plant, down)) {
      num_touching++;
      stack[++stack_pointer] = down;
    }

    if (!is_plot_on_left_edge(popped) &&
        !is_different_plant(map, plot->plant, left)) {
      num_touching++;
      stack[++stack_pointer] = left;
    }

    if (!is_plot_on_right_edge(popped, map) &&
        !is_different_plant(map, plot->plant, right)) {
      num_touching++;
      stack[++stack_pointer] = right;
    }

    perimiter += 4 - num_touching;
  }

  return (Region){.area = area, .perimiter = perimiter, .sides = sides};
}

FencingQuote fencing_quote(Txt *map) {
  const size_t num_columns = strlen(map->lines[0]);
  const size_t num_plots = map->num_lines * num_columns;

  const GardenMap garden_map = {
      .plots = map->lines, .rows = map->num_lines, .columns = num_columns};

  bool visited[num_plots];
  for (size_t plot = 0; plot < num_plots; plot++) {
    visited[plot] = false;
  }

  uint64_t cost = 0;
  uint64_t discount = 0;

  for (size_t row = 0; row < map->num_lines; row++) {
    for (size_t col = 0; col < num_columns; col++) {
      if (!visited[(row * num_columns) + col]) {
        const GardenPlot plot = {.plant = map->lines[row][col],
                                 .location = {.row = row, .column = col}};

        Region region = find_region(&garden_map, &plot, visited);
        uint64_t region_cost = region.area * region.perimiter;
        uint64_t region_discount = region_cost - (region.area * region.sides);

        cost += region_cost;
        discount += region_discount;
      }
    }
  }

  for (size_t plot = 0; plot < num_plots; plot++) {
    assert(visited[plot] && "plot not visited");
  }

  return (FencingQuote){.cost = cost, .discount = discount};
}

int main(void) {
  Txt *map = txt_read(stdin);
  FencingQuote quote = fencing_quote(map);

  txt_free(map);

  print_day(12, "Garden Groups");
  print_part(1, quote.cost, PART1_ANSWER);
  print_part(2, quote.cost - quote.discount, PART2_ANSWER);

  return 0;
}
