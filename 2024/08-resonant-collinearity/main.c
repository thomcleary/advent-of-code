/*
Day 8: Resonant Collinearity
https://adventofcode.com/2024/day/8
*/

#define _DEFAULT_SOURCE
#include <assert.h>
#include <ctype.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../lib/aoc.h"
#include "../lib/txt.h"

// #define USE_EXAMPLE

#ifdef USE_EXAMPLE
#define PART1_ANSWER 14
#define PART2_ANSWER 34
#else
#define PART1_ANSWER 357
#define PART2_ANSWER 1266
#endif

typedef struct Coord {
  int64_t row, column;
} Coord;

typedef struct Frequency {
  char value;
  Coord *antennas;
  size_t num_antennas;
  size_t antennas_size;
} Frequency;

typedef struct AntennaMap {
  size_t rows, columns;
  Frequency **frequencies;
  size_t num_frequencies;
  size_t frequencies_size;
} AntennaMap;

typedef struct AntinodeMap {
  size_t rows, columns;
  char **map;
  uint64_t num_antinodes;
} AntinodeMap;

typedef struct Antinodes {
  Coord *value;
  size_t length;
} Antinodes;

void frequency_free(Frequency *frequency) {
  free(frequency->antennas);
  free(frequency);
}

Frequency *frequency_new(char value, Coord location) {
  Frequency *frequency = malloc(sizeof(*frequency));
  assert(frequency != NULL && "malloc failed");

  frequency->value = value;
  frequency->num_antennas = 1;
  frequency->antennas_size = 8;
  frequency->antennas =
      malloc(sizeof(*frequency->antennas) * frequency->antennas_size);
  assert(frequency->antennas != NULL && "malloc failed");
  frequency->antennas[0] = location;

  return frequency;
}

void frequency_add_antenna(Frequency *frequency, Coord location) {
  if (frequency->num_antennas == frequency->antennas_size) {
    frequency->antennas_size *= 2;
    frequency->antennas =
        realloc(frequency->antennas,
                sizeof(*frequency->antennas) * frequency->antennas_size);
    assert(frequency->antennas != NULL && "realloc failed");
  }
  frequency->antennas[frequency->num_antennas++] = location;
}

bool antinode_is_within_limits(Coord antinode, Coord limit) {
  return antinode.row >= 0 && antinode.column >= 0 &&
         antinode.row <= limit.row && antinode.column <= limit.column;
}

Antinodes frequency_antinodes(Frequency *frequency, Coord limit,
                              bool resonant_harmonics) {
  assert(frequency->num_antennas > 1 && "frequency has less than 2 antinodes");

  size_t num_pairs =
      (frequency->num_antennas * (frequency->num_antennas - 1)) / 2;

  size_t antinodes_size = num_pairs * 2;
  Antinodes antinodes = {.value =
                             malloc(sizeof(*antinodes.value) * antinodes_size),
                         antinodes.length = 0};
  assert(antinodes.value != NULL && "malloc failed");

  for (size_t i = 0; i < frequency->num_antennas; i++) {
    Coord first = frequency->antennas[i];

    for (size_t j = 0; j < frequency->num_antennas; j++) {
      if (j == i) {
        continue;
      }

      Coord second = frequency->antennas[j];

      Coord diff = {.row = first.row - second.row,
                    .column = first.column - second.column};

      if (!resonant_harmonics) {
        Coord antinode = {.row = first.row + diff.row,
                          .column = first.column + diff.column};

        if (antinode_is_within_limits(antinode, limit)) {
          antinodes.value[antinodes.length++] = antinode;
        }
      } else {
        // The antenna's location will always have an antinode
        Coord antinode = {.row = first.row, .column = first.column};

        while (antinode_is_within_limits(antinode, limit)) {
          if (antinodes.length == antinodes_size) {
            antinodes_size *= 2;
            antinodes.value = realloc(
                antinodes.value, sizeof(*antinodes.value) * antinodes_size);
            assert(antinodes.value != NULL && "realloc failed");
          }

          antinodes.value[antinodes.length++] = antinode;
          antinode.row += diff.row;
          antinode.column += diff.column;
        }
      }
    }
  }

  return antinodes;
}

void antenna_map_free(AntennaMap *map) {
  for (size_t i = 0; i < map->num_frequencies; i++) {
    frequency_free(map->frequencies[i]);
  }
  free(map->frequencies);
  free(map);
}

void antenna_map_print(AntennaMap *map) {
  for (size_t i = 0; i < map->num_frequencies; i++) {
    Frequency *frequency = map->frequencies[i];
    printf("Frequency: [%c]\n", frequency->value);
    printf("-------------------\n");
    for (size_t j = 0; j < frequency->num_antennas; j++) {
      Coord antenna = frequency->antennas[j];
      printf("(%" PRId64 ", %" PRId64 ")\n", antenna.row, antenna.column);
    }
    printf("\n");
  }
}

void antennamap_add_frequency(AntennaMap *map, Frequency *frequency) {
  if (map->num_frequencies == map->frequencies_size) {
    map->frequencies_size *= 2;
    map->frequencies = realloc(map->frequencies, sizeof(*map->frequencies) *
                                                     map->frequencies_size);
    assert(map->frequencies != NULL && "realloc failed");
  }

  map->frequencies[map->num_frequencies++] = frequency;
}

Frequency *antennamap_find_frequency(AntennaMap *map, char frequency) {
  for (size_t i = 0; i < map->num_frequencies; i++) {
    Frequency *existing_frequency = map->frequencies[i];
    if (existing_frequency->value == frequency) {
      return existing_frequency;
    }
  }

  return NULL;
}

AntennaMap *antenna_map_parse(Txt *txt) {
  size_t num_rows = txt->num_lines;
  size_t num_columns = strlen(txt->lines[0]);

  AntennaMap *map = malloc(sizeof(*map));
  assert(map != NULL && "malloc failed");
  map->rows = num_rows;
  map->columns = num_columns;
  map->num_frequencies = 0;
  map->frequencies_size = 8;
  map->frequencies = malloc(sizeof(*map->frequencies) * map->frequencies_size);
  assert(map->frequencies != NULL && "malloc failed");

  for (size_t row = 0; row < num_rows; row++) {
    char *line = txt->lines[row];

    for (size_t col = 0; col < num_columns; col++) {
      char ch = line[col];

      if (isalnum(ch)) {
        Frequency *frequency = antennamap_find_frequency(map, ch);

        assert(row <= INT64_MAX && "too many rows");
        assert(col <= INT64_MAX && "too many columns");
        Coord location = {.row = (int64_t)row, .column = (int64_t)col};

        if (frequency == NULL) {
          antennamap_add_frequency(map, frequency_new(ch, location));
        } else {
          frequency_add_antenna(frequency, location);
        }
      }
    }
  }

  return map;
}

void antinode_map_free(AntinodeMap *map) {
  for (size_t i = 0; i < map->rows; i++) {
    free(map->map[i]);
  }
  free(map->map);
  free(map);
}

void antinode_map_print(AntinodeMap *map) {
  for (size_t row = 0; row < map->rows; row++) {
    printf("%s\n", map->map[row]);
  }
  printf("\n");
}

AntinodeMap *antinode_map_new(size_t rows, size_t columns) {
  AntinodeMap *antinode_map = malloc(sizeof(*antinode_map));
  assert(antinode_map != NULL && "malloc failed");

  antinode_map->rows = rows;
  antinode_map->columns = columns;
  antinode_map->map =
      malloc(sizeof(*antinode_map->map) * antinode_map->columns);
  assert(antinode_map->map != NULL);

  for (size_t i = 0; i < antinode_map->rows; i++) {
    char *row = malloc(antinode_map->columns + 1);
    assert(row != NULL && "malloc failed");

    memset(row, '.', antinode_map->columns);
    row[antinode_map->columns] = '\0';
    antinode_map->map[i] = row;
  }

  return antinode_map;
}

AntinodeMap *antinode_map_generate(AntennaMap *antenna_map,
                                   bool resonant_harmonics) {
  AntinodeMap *antinode_map =
      antinode_map_new(antenna_map->rows, antenna_map->columns);

  assert(antenna_map->rows - 1 <= INT64_MAX && "too many rows");
  assert(antenna_map->columns - 1 <= INT64_MAX && "too many columns");
  int64_t row_limit = (int64_t)antenna_map->rows - 1;
  int64_t column_limit = (int64_t)antenna_map->columns - 1;

  Coord antinode_limit = {.row = row_limit, .column = column_limit};

  for (size_t i = 0; i < antenna_map->num_frequencies; i++) {
    Antinodes antinodes = frequency_antinodes(
        antenna_map->frequencies[i], antinode_limit, resonant_harmonics);

    for (size_t j = 0; j < antinodes.length; j++) {
      Coord antinode = antinodes.value[j];

      if (antinode_map->map[antinode.row][antinode.column] != '#') {
        antinode_map->map[antinode.row][antinode.column] = '#';
        antinode_map->num_antinodes++;
      }
    }

    free(antinodes.value);
  }

  return antinode_map;
}

int main(void) {
  Txt *txt = txt_read(stdin);
  AntennaMap *antenna_map = antenna_map_parse(txt);

  AntinodeMap *antinode_map = antinode_map_generate(antenna_map, false);
  uint64_t num_antinodes = antinode_map->num_antinodes;

  AntinodeMap *antinode_map_with_resonant_harmonics =
      antinode_map_generate(antenna_map, true);
  uint64_t num_antinodes_with_resonant_harmonics =
      antinode_map_with_resonant_harmonics->num_antinodes;

  antinode_map_free(antinode_map);
  antinode_map_free(antinode_map_with_resonant_harmonics);
  antenna_map_free(antenna_map);
  txt_free(txt);

  print_day(8, "Resonant Collinearity");
  print_part(1, num_antinodes, PART1_ANSWER);
  print_part(2, num_antinodes_with_resonant_harmonics, PART2_ANSWER);

  return 0;
}
