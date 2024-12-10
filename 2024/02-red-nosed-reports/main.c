/*
Day 2: Red-Nosed Reports
https://adventofcode.com/2024/day/2
*/

// #define USE_EXAMPLE
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
#include "main.h"

typedef struct Report {
  int64_t *levels;
  size_t length;
  size_t size;
} Report;

void free_report(Report *report) {
  free(report->levels);
  free(report);
}

Report *get_report(void) {
  char *line = NULL;
  size_t line_len = 0;

  errno = 0;
  ssize_t result = getline(&line, &line_len, stdin);
  assert(errno == 0 && "getline failed");

  if (result == -1) {
    return NULL;
  }

  line[strcspn(line, "\n")] = '\0';

  Report *report = malloc(sizeof(*report));
  assert(report != NULL && "malloc failed");
  report->size = 8;
  report->levels = malloc(sizeof(*(report->levels)) * report->size);
  assert(report->levels != NULL && "malloc failed");
  report->length = 0;

  char *line_to_free = line;
  char *token;
  while ((token = strsep(&line, " ")) != NULL) {
    errno = 0;
    int64_t level = strtoll(token, NULL, 10);
    assert(errno == 0 && "strtol failed");

    if (report->length + 1 == report->size) {
      report->size *= 2;
      report->levels =
          realloc(report->levels, sizeof(*(report->levels)) * report->size);
      assert(report->levels != NULL && "realloc failed");
    }

    report->levels[report->length] = level;
    report->length += 1;
  }

  free(line_to_free);

  return report;
}

bool is_gradual_change(int64_t prev_diff, int64_t curr_diff) {
  if (curr_diff < -3 || curr_diff == 0 || curr_diff > 3) {
    return false; // Change is to big
  }

  if (prev_diff == 0) {
    return true; // There is no previous diff
  }

  bool is_decreasing = (prev_diff < 0 && curr_diff < 0);
  bool is_increasing = (prev_diff > 0 && curr_diff > 0);

  return is_decreasing || is_increasing;
}

bool is_report_safe(Report *report) {
  int64_t prev_level = report->levels[0];
  int64_t prev_diff = 0;
  bool is_safe_report = true;

  for (size_t i = 1; i < report->length && is_safe_report; i++) {
    int64_t curr_level = report->levels[i];
    int64_t curr_diff = curr_level - prev_level;
    is_safe_report = is_gradual_change(prev_diff, curr_diff);
    prev_level = curr_level;
    prev_diff = curr_diff;
  }

  return is_safe_report;
}

bool is_report_safe_with_dampener(Report *report) {
  Report *dampened_report = malloc(sizeof(*dampened_report));
  assert(dampened_report != NULL && "malloc failed");

  dampened_report->size = report->length - 1;
  dampened_report->length = dampened_report->size;
  dampened_report->levels =
      malloc(sizeof(*(dampened_report->levels)) * dampened_report->size);
  assert(dampened_report->levels != NULL && "malloc failed");

  bool is_safe_report = false;

  for (size_t skip_level = 0; skip_level < report->length && !is_safe_report;
       skip_level++) {
    for (size_t level = 0, i = 0; level < report->length; level++) {
      if (level != skip_level) {
        dampened_report->levels[i] = report->levels[level];
        i++;
      }
    }

    is_safe_report = is_report_safe(dampened_report);
  }

  free_report(dampened_report);

  return is_safe_report;
}

int main(void) {
  Report *report = NULL;
  uint64_t safe_reports = 0;
  uint64_t dampened_reports = 0;

  while ((report = get_report()) != NULL) {
    if (is_report_safe(report)) {
      safe_reports += 1;
    } else if (is_report_safe_with_dampener(report)) {
      dampened_reports += 1;
    }

    free_report(report);
  }

  print_day(2, "Red-Nosed Reports");
  print_part(1, safe_reports, PART1_ANSWER);
  print_part(2, safe_reports + dampened_reports, PART2_ANSWER);

  return 0;
}
