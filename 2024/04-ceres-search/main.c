/*
Day 4: Ceres Search
https://adventofcode.com/2024/day/4
*/

#define _DEFAULT_SOURCE
#include <assert.h>
#include <errno.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../lib/aoc.h"
#include "../lib/str-utils.h"
#include "../lib/txt.h"

// #define USE_EXAMPLE

#ifdef USE_EXAMPLE
#define PART1_ANSWER 18
#define PART2_ANSWER 9
#else
#define PART1_ANSWER 2406
#define PART2_ANSWER 1807
#endif

Txt *transpose(const Txt *txt) {
  size_t num_columns = strlen(txt->lines[0]);
  Txt *txt_transpose = txt_new(num_columns);

  for (size_t col = 0; col < num_columns; col++) {
    char *transposed_line = malloc(txt->num_lines + 1);
    assert(transposed_line != NULL && "malloc failed");

    for (size_t row = 0; row < txt->num_lines; row++) {
      transposed_line[row] = txt->lines[row][col];
    }

    transposed_line[txt->num_lines] = '\0';
    txt_transpose->lines[col] = transposed_line;
  }

  return txt_transpose;
}

Txt *diagonals(const Txt *txt) {
  size_t num_columns = strlen(txt->lines[0]);
  size_t num_diagonals = num_columns + txt->num_lines - 1;
  Txt *txt_diagonals = txt_new(num_diagonals);

  size_t diag = 0;

  for (size_t col = 0; col < num_columns; col++) {
    size_t cols_left = num_columns - col;
    size_t line_size = cols_left < txt->num_lines ? cols_left : txt->num_lines;

    char *diagonal_line = malloc(line_size + 1);
    assert(diagonal_line != NULL && "malloc failed");

    for (size_t row = 0; col + row < num_columns; row++) {
      diagonal_line[row] = txt->lines[row][col + row];
    }
    diagonal_line[line_size] = '\0';
    txt_diagonals->lines[diag++] = diagonal_line;
  }

  for (size_t row = 1; row < txt->num_lines; row++) {
    size_t rows_left = txt->num_lines - row;
    size_t line_size = rows_left < num_columns ? rows_left : num_columns;

    char *diagonal_line = malloc(line_size + 1);
    assert(diagonal_line != NULL && "malloc failed");

    for (size_t col = 0; col + row < txt->num_lines; col++) {
      diagonal_line[col] = txt->lines[row + col][col];
    }

    diagonal_line[line_size] = '\0';
    txt_diagonals->lines[diag++] = diagonal_line;
  }

  return txt_diagonals;
}

Txt *antidiagonals(const Txt *txt) {
  /* Transpose -> Mirror Horizontally = Rotate 90 Clockwise
  1,2,3    1,4,7    7,4,1
  4,5,6 -> 2,5,8 -> 8,5,2
  7,8,9    3,6,9    9,6,3
  */
  Txt *txt_rotated = transpose(txt);
  for (size_t row = 0; row < txt_rotated->num_lines; row++) {
    char *line = txt_rotated->lines[row];
    size_t left = 0;
    size_t right = strlen(line) - 1;

    while (left < right) {
      char tmp = line[left];
      line[left++] = line[right];
      line[right--] = tmp;
    }
  }

  Txt *txt_antidiagonals = diagonals(txt_rotated);
  txt_free(txt_rotated);

  return txt_antidiagonals;
}

uint64_t occurences(const Txt *txt, const char *word) {
  char *word_rev = str_rev(word);
  uint64_t count = 0;

  for (size_t i = 0; i < txt->num_lines; i++) {
    char *line = txt->lines[i];
    count += str_cntocc(word, line);
    count += str_cntocc(word_rev, line);
  }

  free(word_rev);

  return count;
}

uint64_t wordsearch_count(const Txt *ws, const char *word) {
  uint64_t count = occurences(ws, word);

  Txt *ws_transpose = transpose(ws);
  count += occurences(ws_transpose, word);
  txt_free(ws_transpose);

  Txt *ws_diagonals = diagonals(ws);
  count += occurences(ws_diagonals, word);
  txt_free(ws_diagonals);

  Txt *ws_antidiagonals = antidiagonals(ws);
  count += occurences(ws_antidiagonals, word);
  txt_free(ws_antidiagonals);

  return count;
}

bool is_x(const char *word, char tl, char tr, char bl, char br) {
  char left = word[0];
  char right = word[2];

  bool tl_match = tl == left || tl == right;
  bool tr_match = tr == left || tr == right;
  bool bl_match = bl == left || bl == right;
  bool br_match = br == left || br == right;

  bool diagonal_match = tl_match && br_match && tl != br;
  bool antidiagonal_match = tr_match && bl_match && tr != bl;

  return diagonal_match && antidiagonal_match;
}

uint64_t wordsearch_xcount(const Txt *ws, const char *word) {
  assert(strlen(word) == 3 && "strlen(word) is not 3");
  char centre = word[1];

  size_t num_columns = strlen(ws->lines[0]);
  uint64_t xcount = 0;

  for (size_t row = 1; row < ws->num_lines - 1; row++) {
    for (size_t col = 1; col < num_columns - 1; col++) {
      char ch = ws->lines[row][col];
      if (ch != centre) {
        continue;
      }

      char *line_above = ws->lines[row - 1];
      char *line_below = ws->lines[row + 1];

      char top_left = line_above[col - 1];
      char top_right = line_above[col + 1];
      char bottom_left = line_below[col - 1];
      char bottom_right = line_below[col + 1];

      if (is_x(word, top_left, top_right, bottom_left, bottom_right)) {
        assert(xcount != UINT64_MAX && "xcount overflowed");
        xcount++;
      }
    }
  }

  return xcount;
}

int main(void) {
  Txt *ws = txt_read(stdin);

  uint64_t xmas_count = wordsearch_count(ws, "XMAS");
  uint64_t x_mas_count = wordsearch_xcount(ws, "MAS");

  txt_free(ws);

  print_day(4, "Ceres Search");
  print_part_uint64(1, xmas_count, PART1_ANSWER);
  print_part_uint64(2, x_mas_count, PART2_ANSWER);

  return 0;
}
