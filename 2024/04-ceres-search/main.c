/*
Day 4: Ceres Search
https://adventofcode.com/2024/day/4
*/

// #define USE_EXAMPLE
#define _DEFAULT_SOURCE

#include <assert.h>
#include <errno.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../lib/aoc.h"
#include "main.h"

typedef struct WordSearch {
  char **lines;
  size_t num_lines;
} WordSearch;

WordSearch *wordsearch_new(void) {
  WordSearch *ws = malloc(sizeof(*ws));
  assert(ws != NULL && "malloc failed");

  ws->lines = NULL;
  ws->num_lines = 0;

  return ws;
}

void wordsearch_free(WordSearch *ws) {
  for (int i = 0; i < ws->num_lines; i++) {
    free(ws->lines[i]);
  }
  free(ws->lines);
  free(ws);
}

WordSearch *wordsearch_transpose(const WordSearch *ws) {
  assert(ws->lines != NULL && "ws->lines is NULL");
  assert(ws->lines > 0 && "ws->num_lines less than 1");

  WordSearch *ws_transpose = wordsearch_new();
  size_t num_columns = strlen(ws->lines[0]);

  for (int col = 0; col < num_columns; col++) {
    ws_transpose->lines =
        realloc(ws_transpose->lines,
                sizeof(*ws_transpose->lines) * ws_transpose->num_lines + 1);
    assert(ws_transpose->lines != NULL);

    char *transposed_line = malloc(ws->num_lines + 1);
    assert(transposed_line != NULL);

    for (int row = 0; row < ws->num_lines; row++) {
      transposed_line[row] = ws->lines[row][col];
    }
    transposed_line[ws->num_lines] = '\0';
    ws_transpose->lines[ws_transpose->num_lines] = transposed_line;
    ws_transpose->num_lines++;
  }

  return ws_transpose;
}

WordSearch *wordsearch_diagonals(const WordSearch *ws) {
  assert(ws->lines != NULL && "ws->lines is NULL");
  assert(ws->lines > 0 && "ws->num_lines less than 1");

  WordSearch *ws_diagonals = wordsearch_new();
  size_t num_columns = strlen(ws->lines[0]);

  for (int col = 0; col < num_columns; col++) {
    ws_diagonals->lines =
        realloc(ws_diagonals->lines,
                sizeof(*ws_diagonals->lines) * ws_diagonals->num_lines + 1);
    assert(ws_diagonals->lines != NULL);

    int cols_left = num_columns - col;
    size_t line_size = cols_left < ws->num_lines ? cols_left : ws->num_lines;

    char *diagonal_line = malloc(line_size + 1);
    assert(diagonal_line != NULL);

    for (int row = 0; col + row < num_columns; row++) {
      diagonal_line[row] = ws->lines[row][col + row];
    }
    diagonal_line[line_size] = '\0';
    ws_diagonals->lines[ws_diagonals->num_lines] = diagonal_line;
    ws_diagonals->num_lines++;
  }

  for (int row = 1; row < ws->num_lines; row++) {
    ws_diagonals->lines =
        realloc(ws_diagonals->lines,
                sizeof(*ws_diagonals->lines) * ws_diagonals->num_lines + 1);
    assert(ws_diagonals->lines != NULL);

    int rows_left = ws->num_lines - row;
    size_t line_size = rows_left < num_columns ? rows_left : num_columns;

    char *diagonal_line = malloc(line_size + 1);
    assert(diagonal_line != NULL);

    for (int col = 0; col + row < ws->num_lines; col++) {
      diagonal_line[col] = ws->lines[row + col][col];
    }
    diagonal_line[line_size] = '\0';
    ws_diagonals->lines[ws_diagonals->num_lines] = diagonal_line;
    ws_diagonals->num_lines++;
  }

  return ws_diagonals;
}

WordSearch *wordsearch_antidiagonals(const WordSearch *ws) {
  assert(ws->lines != NULL && "ws->lines is NULL");
  assert(ws->lines > 0 && "ws->num_lines less than 1");

  /* Transpose -> Mirror Horizontally = Rotate 90 Clockwise
  1,2,3    1,4,7    7,4,1
  4,5,6 -> 2,5,8 -> 8,5,2
  7,8,9    3,6,9    9,6,3
  */

  WordSearch *ws_rotated = wordsearch_transpose(ws);
  for (int row = 0; row < ws_rotated->num_lines; row++) {
    char *line = ws_rotated->lines[row];
    size_t left = 0;
    size_t right = strlen(line) - 1;

    while (left < right) {
      char tmp = line[left];
      line[left] = line[right];
      line[right] = tmp;
      left++;
      right--;
    }
  }

  WordSearch *ws_antidiagonals = wordsearch_diagonals(ws_rotated);
  wordsearch_free(ws_rotated);

  return ws_antidiagonals;
}

// TODO: move to /lib?
char *str_reverse(const char *str) {
  if (str == NULL) {
    return NULL;
  }

  size_t len = strlen(str);
  char *reversed = malloc(len + 1);
  assert(reversed != NULL);

  for (int i = 0; i < len; i++) {
    reversed[i] = str[len - 1 - i];
  }
  reversed[len] = '\0';

  return reversed;
}

// TODO: move to /lib?
long occurences(const char *word, char *line) {
  assert(word != NULL && "word is NULL");
  assert(*word && "word is empty string");
  assert(line != NULL && "line is NULL");

  size_t word_len = strlen(word);
  long count = 0;
  char *occurence = NULL;

  while ((occurence = strstr(line, word)) != NULL) {
    count++;
    line = occurence + word_len;
  }

  return count;
}

long wordsearch_occurences(const WordSearch *ws, const char *word) {
  const char *word_reverse = str_reverse(word);
  long count = 0;

  for (int i = 0; i < ws->num_lines; i++) {
    char *line = ws->lines[i];
    count += occurences(word, line);
    count += occurences(word_reverse, line);
  }

  return count;
}

long wordsearch_count(const WordSearch *ws, const char *word) {
  assert(ws != NULL && "ws is NULL");
  assert(ws->lines != NULL && "ws->lines is NULL");
  assert(word != NULL && "word is NULL");
  assert(*word && "word is empty string");

  long count = wordsearch_occurences(ws, word);

  WordSearch *ws_transpose = wordsearch_transpose(ws);
  count += wordsearch_occurences(ws_transpose, word);
  wordsearch_free(ws_transpose);

  WordSearch *ws_diagonals = wordsearch_diagonals(ws);
  count += wordsearch_occurences(ws_diagonals, word);
  wordsearch_free(ws_diagonals);

  WordSearch *ws_antidiagonals = wordsearch_antidiagonals(ws);
  count += wordsearch_occurences(ws_antidiagonals, word);
  wordsearch_free(ws_antidiagonals);

  return count;
}

// TODO: add `char **read_lines(FILE *stream)` to /lib?
WordSearch *read_wordsearch(void) {
  WordSearch *ws = wordsearch_new();
  char *line = NULL;
  size_t buf_len = 0;

  errno = 0;
  while (getline(&line, &buf_len, stdin) != -1) {
    ws->lines = realloc(ws->lines, sizeof(*ws->lines) * ws->num_lines + 1);
    assert(ws->lines != NULL && "realloc failed");

    line[strcspn(line, "\n")] = '\0';
    ws->lines[ws->num_lines] = strdup(line);
    ws->num_lines++;
  }
  assert(errno == 0 && "getline failed");

  free(line);

  return ws;
}

int main(void) {
  WordSearch *ws = read_wordsearch();
  long word_count = wordsearch_count(ws, "XMAS");
  wordsearch_free(ws);

  print_day(4, "Ceres Search");
  printf("Part 1: %ld\n", word_count);

  assert(word_count == PART1_ANSWER);
  // assert(0 == PART2_ANSWER);

  return 0;
}
